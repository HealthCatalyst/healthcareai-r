#' @title
#' Generate ROC curve for a dataset.
#' @description Generates ROC curve and AUC for Sensitivity/Specificity or 
#' Precision/Recall.
#' @param predictions A vector of predictions from a machine learning model.
#' @param labels A vector of the true labels. Must be the same length as 
#' predictions.
#' @param aucTYPE Can be "SS" or "PR". Defaults to SS.
#' @param plotFLG Binary value controlling plots Defaults to TRUE (yes).
#' @return auc Integral AUC of ROC plot.
#'
#' @import ROCR
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' 

# functionalized version of AUC function
generateAUC <- function(predictions, labels, aucTYPE, plotFLG) {
  # Error check for uneven length predictions and labels
  if (length(predictions) != length(labels)) {
    stop('Data vectors are not equal length!')
  }
  
  # use SS if aucTYPE is missing
  if (missing(aucTYPE)) {
    aucTYPE <- 'SS'
  }
  
  # plot ROC curve if flag is missing
  if (missing(plotFLG)) {
    plotFLG <- TRUE
  }
  
  # generate ROC data
  roc1 <- prediction( predictions, labels)
  
  # get performance and AUC from either curve type
  if (aucTYPE == 'PR') {
    # ROC Curve Precision/Recall
    perf <- performance(roc1, "prec", "rec")
    perf.auc <- performance(roc1, measure = "auc")
    cat("Area under the curve: ", perf.auc@y.values[[1]])
  }
# defaults to SS
  else {
    perf <- performance(roc1, "tpr","fpr")
    perf.auc <- performance(roc1, measure = "auc")
    cat("Area under the curve: ", perf.auc@y.values[[1]])
    }
  
  # plot AUC 
  if (plotFLG == TRUE){
    plot(perf, col = "blue", lwd = 2)
  }
  
  # return AUC
  return(perf.auc@y.values[[1]])
}