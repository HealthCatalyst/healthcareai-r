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
#' @description With wide datasets (datasets with many columns), sometimes it is
#'   difficult to make sense of the output from \code{\link{missingness}}. This
#'   function is designed to summarize this output with three points. 1. The
#'   percent of variables that contain missingness above the given `threshold`,
#'   2. The variable name with the maximum amount of missingness along with its
#'   percent missingness, and 3. Lists the `top_n` variables that have
#'   missingness over the `threshold` given. This function will also throw a
#'   warning 1. if `top_n` is more than the amount of variables in `x`, and/or
#'   2. if `top_n` is greater than the amount of variables found with
#'   missingness percentages over `threshold`. When the warning is thrown
#'   `top_n` is corrected.
#'
#' @param x Data frame from \code{\link{missingness}}
#' @param threshold a value between 0 and 100 (default = 0). This function will
#'   look for missingness percent values that are greater than this threshold.
#' @param top_n a value between 1 and number of variables (default = 10). This
#'   function will list this number of variables if they contain missingness.
#'   Otherwise, this function will return all the variables that contain
#'   missingness.
#' @return a list of, invisibly
#' @export
#' @examples
#' missingness(pima_diabetes) %>%
#'   summary()
#'
#' # To list the amount of variables with more than 5% missingness
#' missingness(pima_diabetes) %>%
#'   summary(threshold = 5)
#'
#' # To list only the top 3 variables that contain missingness
#' missingness(pima_diabetes) %>%
#'   summary(top_n = 3)
#'
summary.missingness <- function(x, threshold = 0, top_n = 10, ...) {
  if (!length(x))
    stop("`x` is empty.")
  if (threshold < 0 || threshold > 100)
    stop("`threshold` needs to be between 0 and 100")
  if (top_n <= 0)
    stop("`top_n` has to be positive")

  n_var <- length(x$variable)
  if (top_n > n_var) {
    warning(". Reset `top_n` to ", n_var, " variables.")
    top_n <- n_var
  }

  tmp_max_df <- x[x$percent_missing == max(x$percent_missing),]

  max_col <- tmp_max_df[[1]][1]# Grab the first variable if there is a tie
  max_perc <- tmp_max_df[[2]][1]# Grab the first value if there is a tie

  col_missing <- x$percent_missing > threshold
  n_col_missing <- sum(col_missing)
  perc_col_missing <- mean(col_missing) * 100 # Convert to percent

  if (top_n > n_col_missing) {
    warning("`top_n` specifies to list ", top_n, " variables, but only ",
            n_col_missing, " column(s) have/has missingness above the chosen ",
            " threshold. Only ", n_col_missing, " variable(s) is/are listed.")
    top_n <- n_col_missing
  }

  first_nvar <- x$variable[order(x$percent_missing, decreasing = TRUE)][1:top_n]

  if (n_col_missing == 0) {
    # different wording when threshold is 0
    if (threshold == 0)
      out <- paste0("`x` has no variables with missingness.")
    else
      out <- paste0("`x` has no variables with missingness over ", threshold, "%.")
  } else {
    out <- paste0("Missingness summary:\n1: ", perc_col_missing,
                  "% of data variables contain")
    if (threshold != 0)
      out <- paste0(out, " more than ", threshold, "%")
    out <- paste0(out, " missingness.\n2: `", max_col,
                  "` contains the most missingness with ", round(max_perc, 1),
                  "% missingness.\n3: The top ")
    out <-
      if (top_n == 1) { # different wording when one variable found
        paste0(out, "missingness variable is: ", first_nvar, ".\n")
      } else {
        paste0(out, top_n, " missingness variables are: ",
               list_variables(first_nvar), ".\n")
      }
  }

  cat(out)
  return(invisible(out))
}

