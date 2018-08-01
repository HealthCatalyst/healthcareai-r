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
    purrr::map(d, ~ to_search[to_search %in% .x[!is.na(.x)]]) %>%
    unlist() %>%
    unique()
  if (length(possible_na)) {
    possible_na <- map_chr(possible_na, function(st) paste0('"', st, '"'))
    warning("Found these strings that may represent missing values: ",
            list_variables(possible_na),
            ". If they do represent missingness, replace them with NA with: ",
            "`make_na(my_df, c(",
            paste(possible_na, collapse = ", "),
            "))`")
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


#' Replace missingness values with NA and correct columns types
#'
#' @description This function replaces given missingness values with NA in a
#'   given dataframe or tibble. Numeric vectors that were originally loaded as
#'   character or factor vectors (because of missingness values in the column),
#'   are also converted to numeric vectors when values are replaced.
#'
#' @param d A dataframe or tibble
#' @param to_replace A value or vector of values that will be replaced with NA
#' @param drop_levels If TRUE (default) unused factor levels are dropped
#' @return A tibble where the missing value/values is/are replaced with NA
#'
#' @export
#' @examples
#' dat <- data.frame(gender = c("male", "male", "female", "male", "missing"),
#'               name = c("Paul", "Jim", "Sarah", "missing", "Alex"),
#'               weight = c(139, 0, 193, 158, 273))
#'
#' # Replace "missing" in `dat`
#' make_na(dat, "missing")
#'
#' # To keep original levels in factor vectors, set `drop_levels` to `FALSE`.
#' # In this example, this means that `name` and `gender` factor vectors will
#' # still contain "missing" in their levels even though these vectors do not
#' # contain "missing" anymore.
#' make_na(dat, "missing", drop_levels = FALSE)
#'
#' # If there are multiple missing values, pass them through a vector.
#' dat <- data.frame(gender = c("male", "??", "female", "male", "NULL"),
#'               age = c(64, 52, 75, "NULL", 70),
#'               weight = c(139, 0, 193, "??", 273),
#'               stringsAsFactors = FALSE)
#'
#' make_na(dat, c("??", "NULL"))
#'
#' # Run `missingness()` to find possible missingness values in `dat`. It will
#' # suggest the default implementation of `make_na` to replace all found
#' # missingness values (the suggested default implementation is
#' # `make_na(dat, c("??", "NULL"))`).
#' missingness(dat)
#' make_na(dat, c("??", "NULL"))
#'
#' # Note: In this last example, `age` should be loaded as a numeric vector, but
#' # since "NULL" is present, it is stored as a character vector. When "NULL" is
#' # replaced, `age` will be converted to a numeric vector.
#'
make_na <- function(d, to_replace, drop_levels = TRUE) {
  if (!is.data.frame(d))
    stop("\"d\" must be a dataframe or tibble")
  if (!is.atomic(to_replace))
    stop("\"to_replace\" must be atomic")

  d <- purrr::map_dfc(d, ~ {
    .x[.x %in% to_replace] <- NA

    # The following operations execute only on non-numeric columns for speed
    if (!is.numeric(.x)) {
      # Convert factors to characters because factors loose information in
      # as.numeric(). `new_col` is created so that if column cannot be numeric,
      # the column will remain as factor.
      if (is.factor(.x)) {
        new_col <- as.character(.x)
      } else {
        new_col <- .x
      }

      # The column contains numeric values if `as.numeric()` does not coerce the
      # value to NA
      suppressWarnings(
        ans <- sum(is.na(new_col)) == sum(is.na(as.numeric(new_col)))
      )

      if (ans)
        .x <- as.numeric(new_col)
    }
    .x
  })

  if (drop_levels)
    d <- droplevels(d)

  return(d)
}
