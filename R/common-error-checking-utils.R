#' @title 
#' Boolean. Tests whether predictedCol is Y/N
#' 
#' @description Returns a logical, TRUE or FALSE, depending on what is
#' contained in the df or vector
#' @param x A data frame column, vector, or matrix column
#' @return TRUE or FALSE depending on test
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' dat <- data.frame(a = c(0,1,1,0,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0))
#' dat2 <- data.frame(a = c(3, 4, 5, 6, 7, 8, 9))
#' dat3 <- data.frame(a = c('Y', 'N', 'Y', 'N'))
#' isnotYN(dat[, 1])
#' isnotYN(dat2[, 1]) 
#' isnotYN(dat3[, 1])              
#'                 
isnotYNOrNA <- function(x) {
  '%nin%' = Negate('%in%')
  boolResult <- any(x %nin% c('Y', 'N', NA))
  boolResult
}