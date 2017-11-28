#' @title
#' Generate performance metrics after model has been trained
#' @description Generates AU_ROC and AU_PR (including 95% confidence)
#' @param predictions A vector of predictions from a machine learning model.
#' @param ytest A vector of the true labels. Must be the same length as 
#' predictions.
#' @param type A string. Indicates model type and can be "regression" or 
#' "classification". 
#' Defaults to SS.
#' @return Curves (if classification); otherwise nothing. Prints results.
#'
#' @import ROCR
#' @import pROC
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
calculatePerformance <- function(predictions, ytest, type) {
  
  # These are returned for plotting
  ROCPlot <- NULL
  PRCurvePlot <- NULL
  
  # These are returned for unit tests
  AUROC <- NULL
  AUPR <- NULL
  RMSE <- NULL
  MAE <- NULL
  
  if (type == 'classification') {
    
    # Performance curves for return and plotting
    myOutput <- generateAUC(predictions, ytest, 'SS')
    AUROC = myOutput[[1]]
    ROCPlot = myOutput[[3]]
    ROCConf <- pROC::roc(ytest~predictions) # need pROC for 95% confidence
    conf <- pROC::auc(ROCConf) 
    cat(sprintf('95%% CI AU_ROC: (%0.2f , %0.2f) \n', ci(conf)[1], ci(conf)[3]))
    cat(sprintf('\n'))
    
    # Performance AUC calcs (AUPR is ROCR-based)
    myOutput <- generateAUC(predictions, ytest, 'PR')
    AUPR = myOutput[[1]]
    PRCurvePlot = myOutput[[3]]
    ROCConf <- pROC::roc(ytest~predictions) # need pROC for 95% confidence
    AUROC <- pROC::auc(ROCConf)   
    cat(sprintf('\n')) 
    
  } else if (type == 'regression') {
    
    RMSE <- sqrt(mean((ytest - predictions) ^ 2))
    MAE <- mean(abs(ytest - predictions))
    
    cat(paste0('RMSE: ', round(RMSE, 8)), '\n')
    cat(paste0('MAE: ', round(MAE, 8)), '\n')
  }
  
  return(list(ROCPlot,PRCurvePlot,AUROC,AUPR,RMSE,MAE))
}

#' @title
#' Generate ROC or PR curve for a dataset.
#' @description Generates ROC curve and AUC for Sensitivity/Specificity or 
#' Precision/Recall.
#' @param predictions A vector of predictions from a machine learning model.
#' @param labels A vector of the true labels. Must be the same length as 
#' predictions.
#' @param aucType A string. Indicates AUC_ROC or AU_PR and can be "SS" or "PR". 
#' Defaults to SS.
#' @param plotFlg Binary value controlling plots. Defaults to FALSE (no).
#' @param allCutoffsFlg Binary value controlling list of all thresholds. 
#' Defaults to FALSE (no).
#' @return AUC: A number between 0 and 1. Integral AUC of chosen plot type.
#' @return IdealCutoffs: Array of cutoff and associated TPR/FPR or pre/rec.
#' @return Performance: ROCR performance class containing all ROC information.
#'
#' @import ROCR
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' 
#' # generate data
#' # example probablities
#' df <- data.frame(a = rep( seq(0,1,by=0.1), times=9))
#' # example ground truth values
#' df[,'b'] <- (runif(99,0,1)*df[,'a']) > 0.5
#' 
#' # prepare vectors
#' pred <- df[,'a']
#' labels <- df[,'b']
#' 
#' # generate the AUC
#' auc = generateAUC(predictions = pred, 
#'                   labels = labels,
#'                   aucType = 'SS',
#'                   plotFlg = TRUE,
#'                   allCutoffsFlg = TRUE)
generateAUC <- function(predictions, 
                        labels, 
                        aucType='SS', 
                        plotFlg=FALSE, 
                        allCutoffsFlg=FALSE) {
  
  # Error check for uneven length predictions and labels
  if (length(predictions) != length(labels)) {
    stop('Data vectors are not equal length!')
  }
  
  aucType <- toupper(aucType)
  
  # default to SS if something else is entered
  if (aucType != 'SS' && aucType != 'PR') {
    cat('Drawing ROC curve with Sensitivity/Specificity', '\n')
    aucType <- 'SS'
  }
  
  # generate ROC data
  pred = ROCR::prediction(predictions, labels)
  
  # get performance and AUC from either curve type
  if (aucType == 'PR') {
    perf <- ROCR::performance(pred, "prec", "rec")
    x <- as.numeric(unlist(perf@x.values))
    y <- as.numeric(unlist(perf@y.values))
    
    # Convert NaNs to zero
    y[ is.nan(y) ] <- 0
    # From: http://stackoverflow.com/a/30280873/5636012
    area <- sum(diff(x) * (head(y,-1) + tail(y,-1)))/2
    
    # print threshholds and AUC
    cat(sprintf("Area under the PR curve is: %0.2f \n", area))
    
  } else if (aucType == 'SS') {
    perf <- ROCR::performance(pred, "tpr","fpr")
    perf.auc <- ROCR::performance(pred, measure = "auc")
    area <- perf.auc@y.values[[1]]
    
    # print AUC
    cat(sprintf("Area under the ROC curve is: %0.2f \n", area)) 
  }
  
  if (aucType == 'SS') {
    titleTemp <- 'ROC'
    xtitle <- 'False Positive Rate'
    ytitle <- 'True Positive Rate'
  } else {
    titleTemp <- 'PR Curve'
    xtitle <- 'Recall'
    ytitle <- 'Precision'
  }
  
  # plot AUC 
  if (isTRUE(plotFlg)) {
    plot(x = perf@x.values[[1]],
         y = perf@y.values[[1]],
         type = 'l',
         col = "blue", 
         lwd = 2, 
         main = titleTemp,
         xlab = xtitle,
         ylab = ytitle)
  }
  
  # get ideal cutoff values.
  IdealCuts <- getCutOffList(perf = perf, 
                             aucType = aucType, 
                             allCutoffsFlg = allCutoffsFlg)
  
  return(list('AUC' = area, 'IdealCutoffs' = IdealCuts, 'Performance' = perf))
}

#' @title
#' Function to return ideal cutoff and TPR/FPR or precision/recall.
#'
#' @description Calculates ideal cutoff by proximity to corner of the ROC curve.
#' Usually called from \code{\link{generateAUC}}
#' @param perf An ROCR performance class. (Usually made by generateAUC)
#' @param aucType A string. Indicates AUC_ROC or AU_PR and can be "SS" or "PR". 
#' Defaults to SS.
#' @param allCutoffsFlg Binary value controlling list of all thresholds. 
#' @return Array of ideal cutoff and associated TPR/FPR or pre/rec.
#' 
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
getCutOffList = function(perf, aucType = 'SS', allCutoffsFlg = FALSE) {
  ## TODO: Give user the ability to give higher weight to recall or FPR
  x <- unlist(perf@x.values)
  y <- unlist(perf@y.values)
  p <- unlist(perf@alpha.values)
  # for ROC curves
  if (aucType == 'SS') {
    d = (x - 0) ^ 2 + (y - 1) ^ 2
    best <- which(d == min(d))
    ind <- best[length(best)]
    tpr = y[[ind]]
    fpr = x[[ind]]
    cutoff = p[[ind]]
    cat(sprintf("Ideal cutoff is %0.2f, yielding TPR of %0.2f and FPR of %0.2f \n", 
                cutoff, tpr, fpr))  
    if (isTRUE(allCutoffsFlg)) {
      cat(sprintf("%-7s %-6s %-5s \n", 'Thresh', 'TPR', 'FPR'))
      cat(sprintf("%-7.2f %-6.2f %-6.2f \n", p, y, x)) }
    return(c(cutoff, tpr, fpr)) # list of integers
    # for PR curves
  } else if (aucType == 'PR') { 
    d = (x - 1) ^ 2 + (y - 1) ^ 2
    # Convert NaNs to one
    d[ is.nan(d) ] <- 1
    best <- which(d == min(d))
    ind <- best[length(best)]
    pre = y[[ind]]
    rec = x[[ind]]
    cutoff = p[[ind]]
    cat(sprintf("Ideal cutoff is %0.2f, yielding Precision of %0.2f and Recall of %0.2f \n", 
                cutoff, pre, rec))  
    if (isTRUE(allCutoffsFlg)) {
      cat(sprintf("%-7s %-10s %-10s \n", 'Thresh', 'Precision', 'Recall'))
      cat(sprintf("%-7.2f %-10.2f %-10.2f \n", p, y, x)) }
    return(c(cutoff, pre, rec)) # list of integers
  }
}
