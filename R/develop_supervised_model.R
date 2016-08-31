# Import the common functions.
source('R/common.R')

#' Compare predictive models, created on your data
#'
#' @description This step allows one to create test models on your data
#' and helps determine which performs best.
#' @docType class
#' @import caret
#' @import doParallel
#' @import e1071
#' @import grpreg
#' @import pROC
#' @importFrom R6 R6Class
#' @import ranger
#' @import ROCR
#' @import RODBC
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grain.col The dataframe's column that has IDs pertaining to the grain
#' @param predicted.col Column that you want to predict.
#' @param impute Set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://products.healthcatalyst.com/Predictive}
#' @seealso \code{\link{DeploySupervisedModel}}
#' @seealso \code{\link{HCRTools}}
#' @examples
#' # The examples will run as-is, but you can find the data used here
#' # C:\Users\levi.thatcher\Documents\R\win-library\3.2\HCRTools\extdata OR
#' # C:\Program Files\R\R-3.2.3\library\HCRTools\extdata
#'
#' #### Example using iris dataset ####
#' ptm <- proc.time()
#' library(HCRTools)
#'
#' data(iris)
#' head(iris)
#'
#' set.seed(43)
#'
#' p <- SupervisedModelParameters$new()
#' p$df = iris
#' p$type = 'REGRESSION'
#' p$impute = TRUE
#' p$grainCol = ''
#' p$predictedCol = 'Sepal.Width'
#' p$debug = FALSE
#' p$varImp = TRUE
#' p$printResults = TRUE
#' p$cores = 1
#'
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
#' rf$run()
#'
#' # Run GroupLasso
#' grlasso <- GroupLasso$new(p)
#' grlasso$run()
#'
#' print(proc.time() - ptm)
#'
#'
#' #### Example using csv data ####
#' library(HCRTools)
#' #setwd("C:/Your/script/location") # Needed if using YOUR CSV file
#' ptm <- proc.time()
#'
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", "HREmployeeDev.csv", package = "HCRTools")
#'
#' totaldf <- read.csv(file = csvfile, #<-- Replace with 'your/path'
#'                     header = TRUE,
#'                     na.strings = 'NULL')
#'
#' head(totaldf)
#'
#' p <- SupervisedModelParameters$new()
#' p$df = totaldf
#' p$type = 'CLASSIFICATION'
#' p$impute = FALSE
#' p$grainCol = ''
#' p$predictedCol = 'SalariedFlag'
#' p$debug = TRUE
#' p$varImp = TRUE
#' p$printResults = TRUE
#' p$cores = 1
#'
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
#' rf$run()
#'
#' # Run GroupLasso
#' grlasso <- GroupLasso$new(p)
#' grlasso$run()
#'
#'
#' #Plot ROCs from both supervised model classes
#' plot(rf$getROC(), col = "red", legacy.axes=TRUE, mar=c(4, 4, 3, 2)+.1)
#' plot(grlasso$getROC(), add = TRUE, col = "blue", lty=2)
#' title(main = "ROC")
#' legend("bottomright",
#'        c("RandomForest", "GroupLasso"),
#'        cex = 0.8,
#'        col = c("red", "blue"),
#'        lty = 1:2,
#'        inset = .1)
#'
#' # For a given true-positive rate, get false-pos rate and 0/1 cutoff
#' rf$getCutOffs(.6)
#' print(proc.time() - ptm)
#'
#'
#' #### Example using SQL Server data ####
#' # This example requires:
#' #    1) That your local SQL Server has AdventureWorks2012 installed
#'
#' ptm <- proc.time()
#' library(HCRTools)
#' library(RODBC)
#'
#' connection.string = "
#' driver={SQL Server};
#' server=localhost;
#' database=AdventureWorks2012;
#' trusted_connection=true
#' "
#'
#' query = "
#' SELECT
#' [OrganizationLevel]
#' ,[MaritalStatus]
#' ,[Gender]
#' ,[SalariedFlag]
#' ,[VacationHours]
#' ,[SickLeaveHours]
#' FROM [AdventureWorks2012].[HumanResources].[Employee]
#' "
#'
#' df <- SelectData(connection.string, query)
#' head(df)
#'
#' set.seed(43)
#'
#' p <- SupervisedModelParameters$new()
#' p$df = df
#' p$type = 'CLASSIFICATION'
#' p$impute = TRUE
#' p$grainCol = ''
#' p$predictedCol = 'MaritalStatus'
#' p$debug = FALSE
#' p$varImp = TRUE
#' p$printResults = TRUE
#' p$cores = 1
#'
#' # Run RandomForest
#' rf <- RandomForest$new(p)
#' rf$run()
#'
#' # Run GroupLasso
#' grlasso <- GroupLasso$new(p)
#' grlasso$run()
#'
#'
#' #Plot ROCs from both supervised model classes
#' plot(rf$getROC(), col = "red", legacy.axes=TRUE, mar=c(4, 4, 3, 2)+.1)
#' plot(grlasso$getROC(), add = TRUE, col = "blue", lty=2)
#' title(main = "ROC")
#' legend("bottomright",
#'        c("RandomForest", "GroupLasso"),
#'        cex = 0.8,
#'        col = c("red", "blue"),
#'        lty = 1:2,
#'        inset = .1)
#'
#' print(proc.time() - ptm)
#'
#' @export
