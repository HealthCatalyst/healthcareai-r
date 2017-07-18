#' @title 
#' Tests whether predictedCol is Y/N. Allows for NAs to be present.
#' 
#' @description Returns a logical, TRUE or FALSE, depending on what is
#' contained in the vector.  If any NAs are present in the vector, they will be
#' removed later on in development with removeRowsWithNAInSpecCol.
#' @param x A data frame column, matrix column, or vector
#' @return A boolean
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' dat <- data.frame(a = c(0,1,1,0,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0))
#' dat2 <- data.frame(a = c(3, 4, 5, 6, 7, 8, 9))
#' dat3 <- data.frame(a = c('Y', 'N', 'Y', 'N'))
#' dat4 <- data.frame(a = c('Y', 'N', 'Y', 'N', NA))
#' isTargetYN(dat[, 1])
#' isTargetYN(dat2[, 1]) 
#' isTargetYN(dat3[, 1])
#' isTargetYN(dat4[, 1])              
#'                 
isTargetYN <- function(x) {
  NAremoved <- subset(x, !is.na(x))
  boolResult <- all(NAremoved %in% c('Y', 'N'))
  boolResult
}