# Import the common functions.
source('R/common.R')

#' Make risk adjusted comparisons between groups/units or years/months
#'
#' @description This class allows you to create a model based on the
#' performance of many groups in a cohort (besides group A, for example) and
#' see how well group A does against what the model would predict.
#' @docType class

#' @param df Dataframe whose columns are used for calc.
#' @param predicted.col Column that you want to predict.
#' @param group.col Column that we'll use to differentiate
#' @param impute Set all-column imputation to F or T.
#'
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://products.healthcatalyst.com/Predictive}
#' @seealso \code{\link{DeploySupervisedModel}}
#' @seealso \code{\link{HCRTools}}
#' @examples
