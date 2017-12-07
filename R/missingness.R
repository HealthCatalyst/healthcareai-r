#' Percent of entries in each column that are missing
#'
#' @description Finds the percent of NAs in a vector or in each column of a dataframe or
#' matrix or in a vector. Possible mis-coded missing values are searched for and a warning issued if they are found.
#' @param d A data frame or matrix
#' @param return_df If TRUE (default) a data frame is returned, which generally makes reading the output easier. If variable names are so long that the data frame gets wrapped poorly, set this to FALSE.
#' @param to_search A vector of strings that might represent missingness. If found in \code{d}, a warning is issued.
#'
#' @return A data frame with two columns: variable names in \code{d} and the percent of entries in each variable that are missing.
#'
#' @export
#' @examples
#' d <- data.frame(x = c("a", "nil", "b"),
#'                 y = c(1, NaN, 3),
#'                 z = c(1:2, NA))
#' missingness(d)
missingness <- function(d,
                        return_df = TRUE,
                        to_search = c("NA", "NAs", "na", "NaN", "99", "999",
                                      "9999", "?", "??", "nil", "NULL", " ", "")
                        ) {
  if (is.matrix(d) | (is.vector(d) && !is.list(d)))
    d <- as.data.frame(d)

  if (!is.data.frame(d))
    stop("The first argument to missingness is a ", class(d),
         " but must be a data frame, matrix, or vector.")


  # Check for possible representations of missingness
  possible_na <-
    purrr::map(d, ~ to_search[to_search %in% .x]) %>%
    unlist() %>%
    unique()
  if (length(possible_na))
    warning("Found these strings that may represent missing values: ",
            paste(possible_na, collapse = ", "),
            ". If they do represent missingness, replace them with NA.")

  miss <-
    d %>%
    purrr::map_int(~sum(is.na(.x))) %>%
    `/`(nrow(d)) %>%
    sort() %>%
    `*`(100) %>%
    round(1)

  if (return_df) {
    miss <-
      miss %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      setNames(c("variable", "percent_missing"))
  }

  return(miss)
}

#' @title
#' Function to find proportion of NAs in each column of a dataframe or matrix
#'
#' @description DEPRICATED. Use \code{\link{missingness}} instead.
#' @param x A data frame or matrix
#' @param userNAs A vector of user defined NA values.
#' @export
countMissingData <- function(x, userNAs = NULL) {  # nolint

  .Deprecated("missingness", "healthcareai")
  missingness(x)
}
