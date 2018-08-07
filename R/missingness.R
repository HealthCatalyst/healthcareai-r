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
#' missingness(d) %>% plot()
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
    purrr::map(d, ~ if (!is.numeric(.x)) to_search[to_search %in% .x[!is.na(.x)]]) %>%
    unlist() %>%
    unique()
  if (length(possible_na)) {
    possible_na <- map_chr(possible_na, function(st) paste0('"', st, '"'))
    warning("Found these strings that may represent missing values: ",
            list_variables(possible_na),
            ". If they do represent missingness, replace them with NA.")
  }

  miss <- sort(100 * purrr::map_int(d, ~sum(is.na(.x))) / nrow(d))

  if (return_df) {
    miss <- tibble::tibble(variable = names(miss), percent_missing = miss)
  }

  return(structure(miss, class = c("missingness", class(miss))))
}

#' Plot missingness
#'
#' @param x Data frame from \code{\link{missingness}}
#' @param remove_zeros Remove variables with no missingness from the plot?
#'   Default = FALSE
#' @param max_char Maximum length of variable names to leave untruncated.
#'   Default = 40; use \code{Inf} to prevent truncation. Variable names longer
#'   than this will be truncated to leave the beginning and end of each variable
#'   name, bridged by " ... ".
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
plot.missingness <- function(x, remove_zeros = FALSE, max_char = 40,
                             title = NULL, font_size = 11, point_size = 3,
                             print = TRUE, ... ) {

  if (inherits(x, "missingness") && !is.data.frame(x))
    x <- data.frame(variable = names(x), percent_missing = x)

  if ( (is.data.frame(x) && names(x) != c("variable", "percent_missing") ) ||
       !is.data.frame(x))
    stop("x must be a data frame from missingness, or at least look like one!")

  if (remove_zeros)
    x <- dplyr::filter(x, percent_missing > 0)

  x$variable <- trunc_char(x$variable, max_char)

  the_plot <-
    x %>%
    ggplot(aes(x = reorder(variable, percent_missing), y = percent_missing)) +
    geom_point(size = point_size, aes(color = percent_missing == 0)) +
    coord_flip() +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = "Percent of Observations Missing",
                       labels = function (x) paste0(x, "%"),
                       limits = c(0, NA)) +
    scale_color_manual(values = c("TRUE" = "darkgray", "FALSE" = "black"),
                       guide = FALSE)
    ggtitle(title) +
    theme_gray(base_size = font_size)

  if (print)
    print(the_plot)
  return(invisible(the_plot))
}

#' @export
print.missingness <- function(x, ...) {
  if (is.data.frame(x)) {
    NextMethod("print", x, n = Inf)
  } else {
    NextMethod("print", x)
  }
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

#' Summarizes data given by \code{\link{missingness}}
#'
#' @description Interpreting \code{\link{missingness}} results from wide
#'   datasets is difficult. This function helps interpret missingness output by
#'   summarizing this output by listing: the percent of variables that
#'   contain missingness, the variable name of the variable with the maximum
#'   amount of missingness along with its percent missingness, and a tibble
#'   that lists the top 5 missingness levels with the count of the number of
#'   variables associated with each level.
#' @param object Data frame from \code{\link{missingness}}
#' @param ... Unused
#' @return a tibble of the top 5 missingness percentage levels with the count of
#'   the number of variables associated with each level
#' @export
#' @examples
#' missingness(pima_diabetes) %>%
#'   summary()
#'
summary.missingness <- function(object, ...) {
  if (!length(object))
    stop("`object` is empty.")

  col_missing <- pull(object, percent_missing) > 0
  if (sum(col_missing) == 0) {
    stop("`object` has no variables with missingness.")
  } else {
    # Get the percent of columns that have any missing values
    perc_col_missing <- mean(col_missing) * 100 # Convert from decimal to percent

    # Get the name and percent_missing of the variable with the most missingness
    max_df <- (
      object %>% filter(percent_missing == max(percent_missing))
    )[1, ] #In tie, grab first row

    out <- paste0("Missingness summary:\n", perc_col_missing,
                  "% of data variables contain missingness.\n`",
                  max_df$variable,
                  "` contains the most missingness with ",
                  round(max_df$percent_missing, 1),
                  "% missingness.\n\nNumber of variables with levels of ",
                  "missingness:\n")

    # Get the top 5 missingness percentage levels with the count of the number
    # of variables associated with each level
    sumry_missing <- (
      object %>%
      count(percent_missing) %>%
      arrange(desc(percent_missing)) %>%
      rename(n_variables = n)
    )[1:5, ] # only grab first 5 rows
  }

  cat(out)
  print(sumry_missing)
  return(invisible(sumry_missing))
}
