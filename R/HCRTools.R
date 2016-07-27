#' HCRTools: a streamlined way to develop and deploy models
#'
#' HCRTools provide a clean interface that lets one create and compare multiple
#' models on your data, and then deploy the model that is most accurate.
#'
#' This is done in a two-step process:
#'
#' \itemize{
#' \item Use \code{\link{DevelopSupervisedModel}} to test and compare models
#' based on your data.
#' \item Once you've determined which model is best, use
#' \code{\link{DeploySupervisedModel}} to create a final model, automatically
#' save it, predict against test data, and push predicted values into SQL
#' Server.
#' }
#
#' @seealso \code{\link{RiskAdjustedComparisons}}
#' @seealso \code{\link{GroupedLOCF}}
#' @seealso \code{\link{SelectData}}
#' @seealso \code{\link{IsBinary}}
#' @seealso \code{\link{ImputeColumn}}
#' @seealso \code{\link{RemoveRowsWithNAInSpecCol}}
#' @seealso \code{\link{FindTrendsAboveThreshold}}
#' @seealso \code{\link{ConvertDateTimeColToDummies}}
#' @docType package
#' @name HCRTools
NULL
