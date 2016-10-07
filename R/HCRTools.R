#' HCRTools: a streamlined way to develop and deploy models
#'
#' HCRTools provide a clean interface that lets one create and compare multiple
#' models on your data, and then deploy the model that is most accurate.
#'
#' This is done in a two-step process:
#'
#' \itemize{
#' \item Use \code{\link{Lasso}} or \code{\link{RandomForest}} to test and
#' compare models based on your data.
#' \item Once you've determined which model is best, use
#' \code{\link{DeployLasso}} or \code{\link{DeployRandomForest}} to create a
#' final model, automatically save it, predict against test data, and push
#' predicted values into SQL Server.
#' }
#' @references \url{http://healthcareml.org/}
#' @seealso \code{\link{Lasso}}
#' @seealso \code{\link{RandomForest}}
#' @seealso \code{\link{RiskAdjustedComparisons}}
#' @seealso \code{\link{imputeColumn}}
#' @seealso \code{\link{groupedLOCF}}
#' @seealso \code{\link{selectData}}
#' @seealso \code{\link{writeData}}
#' @seealso \code{\link{orderByDate}}
#' @seealso \code{\link{isBinary}}
#' @seealso \code{\link{removeRowsWithNAInSpecCol}}
#' @seealso \code{\link{countPercentEmpty}}
#' @seealso \code{\link{removeColsWithAllSameValue}}
#' @seealso \code{\link{returnColsWithMoreThanFiftyCategories}}
#' @seealso \code{\link{findTrends}}
#' @seealso \code{\link{convertDateTimeColToDummies}}
#' @seealso \code{\link{countDaysSinceFirstDate}}
#' @seealso \code{\link{calculateTargetedCorrelations}}
#' @seealso \code{\link{calculateAllCorrelations}}
#' @docType package
#' @name HCRTools
NULL
