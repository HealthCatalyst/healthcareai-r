#' @title 
#' Function to find proportion of NAs in each column of a dataframe or matrix
#' 
#' @description Finds the proportion of NAs in each column of a dataframe or 
#' matrix.  NA possibilities that are considered: NA, NULL, NaN, "?", "nil", 
#' "NA", "NULL", " ", "", "blank","NAs", "na", "??", "empty", "999", "NaN", 
#' "none", "NONE", "None", "nas". 
#' @param x A data frame or matrix
#' @return A numeric vector of the proportion of NAs in each column. Columns 
#' with no NAs are not returned.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' bob <- data.frame(d = c("NULL", NA, "empty", 5, "?", 'nil', "NaN", " "),
#'                   y = c("??", ' ', "999", 999, "tom", "5", 7, 10),
#'                   l = rep(NA, 8),
#'                   a = c("blank", 0, "na", "None", "none", 3, 10, 4),
#'                   n = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
#'                   new = c(1, 2, 3, 4, 5, 6, 7, 8))             
#' nullPointer(bob)
#' 
nullPointer <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("nullPointer must be fed a matrix or a dataframe")
  } else {
    if (is.matrix(x)) {
      x <- as.data.frame(x)
    }
  }
  possibleNAs <- c(NA, NaN, "?", "nil", "NA", "NULL", " ", "", "blank", 
                   "NAs", "na", "??", "empty", "999", "NaN", "none", "NONE", 
                   "None", "nas") 
  
  nullsPresent <- sapply(x, function(z)
  {(sum(z %in% possibleNAs)) / (length(z))} )
  
  return(nullsPresent[nullsPresent != 0])
}