#' @title 
#' Function to find proportion of NAs in each column of a dataframe or matrix
#' 
#' @description Finds the proportion of NAs in each column of a dataframe or 
#' matrix.  NA possibilities that are defined: NA, "NA", "NAs", "na", NaN, "NaN"
#' , "?", "??", "nil", "NULL", " ", "", "999". User has ability to define their 
#' own NA values by using the userNAs parameter. User defined NAs will be added 
#' to the list of already defined NAs.
#' @param x A data frame or matrix
#' @param userNAs A vector of user defined NA values.
#' @return A numeric vector of the proportion of NAs in each column.
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' bob <- data.frame(d = c("NULL", NA, "empty", 5, "?", 'nil', "NaN", " ", 2),
#'                   y = c("??", ' ', "999", 999, "tom", "5", 7, 10, 2),
#'                   l = rep(NA, 9),
#'                   a = c("blank", 0, "na", "None", "none", 3, 10, 4, "what"),
#'                   n = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas", 2),
#'                   new = c(1, 2, 3, 4, 5, "void", 7, 8, "what"))           
#' countMissingData(bob)
#' countMissingData(bob, userNAs = c("void", "what"))
countMissingData <- function(x, userNAs = NULL) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("countMissingData must be fed a matrix or a dataframe")
  } else {
    if (is.matrix(x)) {
      x <- as.data.frame(x)
    }
  }
  
  possibleNAs <- c(NA, "NA", "NAs", "na", NaN, "NaN", "?", "??", "nil", "NULL", 
                   " ", "") 
  
  if (!is.null(userNAs) && !is.vector(userNAs)) {
    stop("User provided NAs must be in vector format")
  } else{
    
    if (!is.null(userNAs)) {
      possibleNAs <- c(userNAs, possibleNAs)
    }
  }
  
  nullsPresent <- round(sapply(x, function(z)
  {(sum(z %in% possibleNAs)) / (length(z))}), 4)
  
  return(nullsPresent)
}