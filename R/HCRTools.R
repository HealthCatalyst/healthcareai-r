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
#' @seealso \code{\link{ImputeColumn}}
#' @seealso \code{\link{GroupedLOCF}}
#' @seealso \code{\link{SelectData}}
#' @seealso \code{\link{WriteData}}
#' @seealso \code{\link{OrderByDate}}
#' @seealso \code{\link{IsBinary}}
#' @seealso \code{\link{RemoveRowsWithNAInSpecCol}}
#' @seealso \code{\link{CountPercentEmpty}}
#' @seealso \code{\link{RemoveColsWithAllSameValue}}
#' @seealso \code{\link{ReturnColsWithMoreThanFiftyCategories}}
#' @seealso \code{\link{FindTrends}}
#' @seealso \code{\link{ConvertDateTimeColToDummies}}
#' @seealso \code{\link{CountDaysSinceFirstDate}}
#' @seealso \code{\link{CalculateTargetedCorrelations}}
#' @seealso \code{\link{CalculateAllCorrelations}}
#' @docType package
#' @name HCRTools
NULL
