#' healthcareai: a streamlined way to develop and deploy models
#'
#' healthcareai provides a clean interface that lets one create and compare multiple
#' models on your data, and then deploy the model that is most accurate. healthcareai
#' also includes functions for data exploration, data cleaning, and model evaluation.
#'
#' This is done in a four-step process:
#'
#' \enumerate{
#' \item{\strong{Load and profile data}}{\cr Use \code{\link{selectData}} to pull data directly 
#' from the SQL database. Then, \code{\link{featureAvailabilityProfiler}} and 
#' \code{\link{countPercentEmpty}} can help determine how many null values are in a 
#' column and how they are populated over time. \code{\link{calculateTargetedCorrelations}} 
#' and \code{\link{findTrends}} can help explore data. Manipulate dates using 
#' \code{\link{orderByDate}} and \code{\link{countDaysSinceFirstDate}}. }
#' 
#' \item{\strong{Develop a machine learning model}}{\cr Use \code{\link{LassoDevelopment}} or
#' \code{\link{RandomForestDevelopment}} and test different combinations of 
#' features. Determine the best model using: 
#' \itemize{
#' \item Area under the ROC curve or area under the Performance-Recall 
#' curve for classification problems (yes or no response). 
#' \item Mean squared error for regression problems (continuous response).
#' }}
#'
#' \item{\strong{Deploy the machine learning model}}{\cr
#' \code{\link{LassoDeployment}} or \code{\link{RandomForestDeployment}} to
#' create a final model, automatically save it, predict against test data, and
#' push predicted values into a SQL environment. This can be tested locally,
#' but eventually lives on the production server.}
#' 
#' \item{\strong{Monitor performance in production environment}}{\cr After 
#' generating predictions and getting ground truth values, use 
#' \code{\link{generateAUC}} to monitor performance over time. This 
#' should happen after greater than 1000 predictions have been made or 30 days
#' have passed.}
#' }
#' 
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
#' @seealso \code{\link{removeColsWithAllSameValue}}
#' @seealso \code{\link{returnColsWithMoreThanFiftyCategories}}
#' @seealso \code{\link{findTrends}}
#' @seealso \code{\link{convertDateTimeColToDummies}}
#' @seealso \code{\link{countDaysSinceFirstDate}}
#' @seealso \code{\link{calculateTargetedCorrelations}}
#' @seealso \code{\link{calculateAllCorrelations}}
#' @seealso \code{\link{featureAvailabilityProfiler}}
#' @seealso \code{\link{generateAUC}}
#' @docType package
#' @name healthcareai
NULL
