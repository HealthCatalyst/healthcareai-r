#' Find missingness in each column and search for strings that might represent
#' missing values
#'
#' @description Finds the percent of NAs in a vector or in each column of a
#'   dataframe or matrix or in a vector. Possible mis-coded missing values are
#'   searched for and a warning issued if they are found.
#' @param d A data frame or matrix
#' @param return_df If TRUE (default) a data frame is returned, which generally
#'   makes reading the output easier. If variable names are so long that the
#'   data frame gets wrapped poorly, set this to FALSE.
#' @param to_search A vector of strings that might represent missingness. If
#'   found in \code{d}, a warning is issued.
#'
#' @return A data frame with two columns: variable names in \code{d} and the
#'   percent of entries in each variable that are missing.
#' @seealso \code{\link{plot.missingness}}
#'
#' @export
#' @examples
#' d <- data.frame(x = c("a", "nil", "b"),
#'                 y = c(1, NaN, 3),
#'                 z = c(1:2, NA))
#' missingness(d)
missingness <- function(d,
                        return_df = TRUE,
                        to_search = c("NA", "NAs", "na", "NaN",
                                      "?", "??", "nil", "NULL", " ", "")
) {
  if (is.matrix(d) | (is.vector(d) && !is.list(d)))
    d <- as.data.frame(d)

  if (!is.data.frame(d))
    stop("The first argument to missingness is a ", class(d),
         " but must be a data frame, matrix, or vector.")

  # Check for possible representations of missingness
  possible_na <-
    purrr::map(d, ~ to_search[to_search %in% .x[!is.na(.x)]]) %>%
    unlist() %>%
    unique()
  if (length(possible_na)) {
    possible_na <- map_chr(possible_na, function(st) paste0('"', st, '"'))
    warning("Found these strings that may represent missing values: ",
            paste(possible_na, collapse = ", "),
            ". If they do represent missingness, replace them with NA.")
  }

  miss <-
    d %>%
    purrr::map_int(~sum(is.na(.x))) %>%
    `/`(nrow(d)) %>%
    sort() %>%
    `*`(100) %>%
    round(1)

  if (return_df) {
    miss <- data.frame(variable = names(miss),
                       percent_missing = miss,
                       stringsAsFactors = FALSE)
    rownames(miss) <- NULL
  }

  return(structure(miss, class = c("missingness", class(miss))))
}

#' Plot missingness
#'
#' @param x Data frame from \code{\link{missingness}}
#' @param filter_zero Remove variables with no missingness from the plot?
#'   Default = FALSE
#' @param title Plot title
#' @param font_size Relative size of all fonts in plot, default = 11
#' @param point_size Size of dots, default = 3
#' @param print Print the plot? Default = TRUE
#' @param ... Unused
#'
#' @return A ggplot object, invisibly.
#' @export
#' @seealso \code{\link{missingness}}
#'
#' @examples
#' pima_diabetes %>%
#'   missingness() %>%
#'   plot()
plot.missingness <- function(x, filter_zero = FALSE,
                             title = NULL, font_size = 11, point_size = 3,
                             print = TRUE, ... ) {

  if (inherits(x, "missingness") && !is.data.frame(x))
    x <- data.frame(variable = names(x), percent_missing = x)

  if ( (is.data.frame(x) && names(x) != c("variable", "percent_missing") ) ||
       !is.data.frame(x))
    stop("x must be a data frame from missingness, or at least look like one!")

  if (filter_zero)
    x <- dplyr::filter(x, percent_missing > 0)
  the_plot <-
    x %>%
    ggplot(aes(x = reorder(variable, percent_missing), y = percent_missing)) +
    geom_point(size = point_size) +
    coord_flip() +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = "Percent of Observations Missing",
                       labels = function (x) paste0(x, "%")) +
    ggtitle(title) +
    theme_gray(base_size = font_size)

  if (print)
    print(the_plot)
  return(invisible(the_plot))
}

#' @title
#' Function to find proportion of NAs in each column of a dataframe or matrix
#'
#' @description DEPRICATED. Use \code{\link{missingness}} instead.
#' @param x A data frame or matrix
#' @param userNAs A vector of user defined NA values.
#' @export
countMissingData <- function(x, userNAs = NULL) {

  .Deprecated("missingness", "healthcareai")
  missingness(x)
}
