#' print method for model_list
#'
#' @param mlist model_list
#'
#' @return the input
#' @noRd
print.model_list <- function(mlist) {
  rinfo <- extract_model_info(mlist)
  hyperp <-
    rinfo$best_model_tune %>%
    sapply(as.character) %>%
    paste(names(.), ., sep = " = ", collapse = "\n  ")
  out <- paste0(
    "Target: ", rinfo$target,
    "\nClass: ", rinfo$m_class,
    "\nAlgorithms Tuned: ", paste(rinfo$algs, collapse = ", "),
    "\nPerformance Metric: ", rinfo$metric,
    "\nNumber of Observations: ", rinfo$ddim[1],
    "\nNumber of Features: ", rinfo$ddim[2] - 1L,

    "\n\nBest model: ", rinfo$best_model_name,
    "\n", rinfo$metric, " = ", round(rinfo$best_model_perf, 2),
    "\nHyperparameter values:", "\n  ", hyperp
  )
  cat(out)
  return(invisible(mlist))
}

#' summary method for model_list
#'
#' @param mlist model_list
#'
#' @return the summary
#' @noRd
summary.model_list <- function(mlist) {
  # Data details: rows, features, outcome class/alg type
  # CV Details: n-folds, param depth
  # Names of algs
  # Best performing alg and hyperparameter values
  # Tables of hyperparamter values and performance
  rinfo <- extract_model_info(mlist)

  names(m[[1]])
  print()
  ncol(m[[1]]$trainingData) - 1

  class(out) <- summary.model_list
  return(out)
}


#' Plot performance of regression models
#'
#' @param rlist regression_list object as returned by \code{\link{tune}}
#' @param print If TRUE (default) plot is printed
#'
#' @return Plot of model performance as a function of algorithm and
#'   hyperparameter values tuned over. Generally called for the side effect of
#'   printing a plot, but the plot is also invisibly returned.
#' @noRd
#' @importFrom cowplot plot_grid
#' @importFrom purrr map_df
#' @importFrom purrr map_chr
plot.regression_list <- function(rlist, print = TRUE) {
  if (!inherits(rlist, "regression_list"))
    stop("rlist is class ", class(rlist)[1], ", but needs to be regression_list")
  bounds <- purrr::map_df(rlist, function(m) range(m$results[[m$metric]]))
  y_range <- c(min(bounds[1, ]), max(bounds[2, ]))
  nrows <- ceiling(length(rlist) / 2)
  gg_list <-
    lapply(rlist, function(mod) {
      # optimum is min or max depending on metric
      optimum <- if (mod$maximize) max else min
      best_metric <- round(optimum(mod$results[[mod$metric]]), 2)
      ggplot(mod) +
        ylim(y_range) +
        labs(title = mod$modelInfo$label,
             caption = paste("Best ", mod$metric, ": ", best_metric))
    })
  gg <- cowplot::plot_grid(plotlist = gg_list)
  if (print)
    print(gg)
  return(invisible(gg))
}

evaluate.regression_list <- function(mlist) {

  return()  # Best model
}

#' Get info from a model_list
#'
#' @param mlist
#'
#' @return list of statistics
#' @noRd
extract_model_info <- function(mlist) {
  # optimum is min or max depending on metric
  optimum <- if (mlist[[1]]$maximize) max else min
  metric <- mlist[[1]]$metric
  best_metrics <- purrr::map_dbl(mlist, ~ optimum(.x$results[[metric]]))
  best_model <- which(best_metrics == optimum(best_metrics))[1] # 1 in case of tie
  algs <- purrr::map_chr(mlist, ~ .x$modelInfo$label)
  m_class <- mlist[[1]]$modelType
  target <- attr(mlist, "target")
  ddim <- dim(mlist[[1]]$trainingData)
  best_model_name <- algs[best_model]
  best_model_perf <- best_metrics[best_model]
  best_model_tune <- mlist[[best_model]]$bestTune
  list(
    m_class = m_class,
    algs = algs,
    target = target,
    metric = metric,
    best_model_name = best_model_name,
    best_model_perf = best_model_perf,
    best_model_tune = best_model_tune,
    ddim = ddim
  )
}
