#' healthcareai: a streamlined way to develop and deploy models
#'
#' healthcareai provides a clean interface that lets one create and compare multiple
#' models on your data, and then deploy the model that is most accurate. healthcareai
#' also includes functions for data exploration, data cleaning, and model evaluation.
#'
#' This is done in a four-step process:
#'
#' \itemize{
#' \item Load and profile data. Use \code{\link{selectData}} to pull data directly 
#' from the SQL database. Then, \code{\link{featureAvailabilityProfiler}} can help
#' determine how many null values are in a column and how they are populated over time.
#' 
#' \item Build a machine learning model using \code{\link{LassoDevelopment}} or
#' \code{\link{RandomForestDevelopment}} and test different combinations of 
#' features. Determine the best model using: 
#' \itemize{
#' \item Area under the ROC curve or area under the performance-recall 
#' curve for classification problems (yes or no response). 
#' \item Mean squared error for regression problems (continuous response).
#' }
#'
#' \item Once you've determined which model is best, use
#' \code{\link{LassoDeployment}} or \code{\link{RandomForestDeployment}} to
#' create a final model, automatically save it, predict against test data, and
#' push predicted values into SQL Server.
#' }
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{LinearMixedModelDevelopment}}
#' @seealso \code{\link{LinearMixedModelDeployment}}
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
#' @name healthcareai
NULL
