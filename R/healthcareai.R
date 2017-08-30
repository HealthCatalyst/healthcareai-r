#' healthcareai: a streamlined way to develop and deploy models
#'
#' healthcareai provides a clean interface to create and compare multiple
#' models on your data and then deploy the model that is most accurate. healthcareai
#' also includes functions for data exploration, data cleaning, and model evaluation.
#'
#' This is done in a three-step process: First, loading, profiling, and feature 
#' engineering. Second, developing a model. Third, deploying and monitoring the
#' model.
#'
#' \enumerate{
#' \item{\strong{Load and profile data}}{\cr
#' \itemize{
#' \item{Loading Data:}
#' \itemize{
#' \item Use \code{\link{selectData}} to pull data directly from a SQL 
#' database}
#' \item{Profiling and Analyzing Data - One can get quite far in healthcare data
#' analysis without ever going beyond this step:}
#' \itemize{
#' \item \code{\link{featureAvailabilityProfiler}} will find how much data is 
#' present in each variable over time.
#' \item \code{\link{countMissingData}} finds the proportion of missing data in 
#' each variable.
#' \item \code{\link{findVariation}} and \code{\link{variationAcrossGroups}} are
#'  used to find variation across/between subgroups of data.
#' \item \code{\link{findTrends}} finds trends that are six months or longer.
#' \item \code{\link{RiskAdjustedComparisons}} compares groups in a risk
#' adjusted fashion. See 
#' \href{http://www.jointcommission.org/assets/1/18/RA_Guide_Risk_Model.pdf}{RiskAdjustement}
#' for a general introduction.
#' \item \code{\link{calculateTargetedCorrelations}} will calculate correlations
#'  for all numeric columns and a specified variable of interest.
#' \item \code{\link{returnColsWithMoreThanFiftyCategories}} shows which
#' categorical columns have more than 50 categories.
#' \item \code{\link{KmeansClustering}} used to cluster data with or without an
#' outcome variable}
#' \item{Feature Engineering:}
#' \itemize{
#' \item \code{\link{convertDateTimeColToDummies}} will convert a date variable
#' into dummy columns of day, hour, etc. For seasonal pattern modeling.
#' \item \code{\link{countDaysSinceFirstDate}} shows days since first day in 
#' input column.
#' \item \code{\link{groupedLOCF}} carries last observed value forward. This is
#' an imputation method for longitudinal data.
#' }}
#' }
#' 
#' \item{\strong{Develop a machine learning model}}{\cr
#' \itemize{
#' \item{Models:}
#' \itemize{
#' \item \code{\link{LassoDevelopment}}: Used for regression or classification 
#' and does an especially good job with a lot of variables.
#' \item \code{\link{RandomForestDevelopment}}: Used for regression or 
#' classification and is well suited to non-linear data.
#' \item \code{\link{XGBoostDevelopment}}: Used for multi-class classification
#' (problems where there are more than 2 classes). Well suited to non-linear 
#' data.
#' \item \code{\link{LinearMixedModelDevelopment}}: Best suited for longitudinal
#' data and datasets with less than 100k rows and 50 variables. Can do
#' classification or regression.
#' }
#' \item{Performance of Trained Models:}
#' \itemize{
#' \item Area under the ROC curve or area under the precision-recall curve are 
#'  used to evaluate the performance of classification models.
#' \item The mean squared error (MSE) and root mean squared error (RMSE) are 
#' used to evaluate the performance of regression problems.
#' }
#' }
#' Note: models are saved in the working directory after creation.
#' }
#'
#' \item{\strong{Deploy and Monitor the Machine Learning Model}}{\cr
#' \itemize{
#' \item{Deploy the Model:}
#' \itemize{
#' \item Use \code{\link{LassoDeployment}}, 
#' \code{\link{LinearMixedModelDeployment}}, 
#' \code{\link{RandomForestDeployment}}, or \code{\link{XGBoostDeployment}} to 
#' load the model from development and predict against test data. The 
#' deployments can be tested locally, but eventually live on the production 
#' server.
#' \item Use \code{\link{writeData}} to push the predicted values into a SQL 
#' environment.}
#' \item{Monitoring the model:}
#' \itemize{
#' \item \code{\link{generateAUC}} is used to monitor performance over time. 
#' This should happen after the predictions can be validated with the result. If
#' you're predicting 30-day readmissions, you can't validate until 30 days have 
#' passed since the predictions.
#' }
#' }
#' }
#' }
#' 
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @references \url{http://healthcare.ai}
#' @docType package
#' @name healthcareai
NULL
