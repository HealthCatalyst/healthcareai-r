#' Copy of the caret function, `prSummary`, so that we can change the output
#' param to "PR" instead of "AUC." Stated Precision and Recall assume a cutoff of 0.5
#' @noRd
#' @importFrom MLmetrics PRAUC
#' @importFrom utils getFromNamespace
pr_summary <- function (data, lev = NULL, model = NULL) {
  # Non-exported functions
  precision.default <- getFromNamespace("precision.default", "caret")
  recall.default <- getFromNamespace("recall.default", "caret")
  F_meas.default <- getFromNamespace("F_meas.default", "caret")
  requireNamespaceQuietStop <- getFromNamespace("requireNamespaceQuietStop", "caret")

  requireNamespaceQuietStop("MLmetrics")
  if (length(levels(data$obs)) > 2)
    stop(paste("Your outcome has", length(levels(data$obs)),
               "levels. `prSummary`` function isn't appropriate.",
               call. = FALSE))
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    stop("Levels of observed and predicted data do not match.",
         call. = FALSE)
  pr_auc <- try(MLmetrics::PRAUC(y_pred = data[, lev[1]],
                                 y_true = ifelse(data$obs == lev[1], 1, 0)), silent = TRUE)
  if (inherits(pr_auc, "try-error"))
    pr_auc <- NA
  c(PR = pr_auc,
    Precision = precision.default(data = data$pred,
                                          reference = data$obs,
                                          relevant = lev[1]),
    Recall = recall.default(data = data$pred,
                                    reference = data$obs,
                                    relevant = lev[1]),
    F = F_meas.default(data = data$pred,
                               reference = data$obs,
                               relevant = lev[1]))
}
