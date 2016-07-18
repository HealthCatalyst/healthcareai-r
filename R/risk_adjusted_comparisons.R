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
#' @references \url{http://products.healthcatalyst.com/Predictive}
#' @seealso \code{\link{HCRTools}}

RiskAdjustedComparisons <- R6Class("RiskAdjustedComparisons",
  public = list(

    # Add attributes here
    df = NA,


  initialize = function(df, predicted.col, group.col, impute) {
    # Clean and impute data



  }
 )
)
