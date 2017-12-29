#' Plot performance of regression models
#'
#' @param rlist regression_list object as returned by \code{\link{tune}}
#' @param print If TRUE (default) plot is printed
#'
#' @return Plot of model performance as a function of algorithm and
#'   hyperparameter values tuned over. Generally called for the side effect of
#'   printing a plot, but the plot is also invisibly returned.
#' @export
#' @importFrom cowplot plot_grid
#' @importFrom purrr map_df
#'
#' @examples
#' m <- tune(mtcars, mpg)
#' plot(m)
plot.regression_list <- function(rlist, print = TRUE) {
  if (!inherits(rlist, "regression_list"))
    stop("rlist is class ", class(rlist)[1], ", but needs to be regression_list")
  bounds <- purrr::map_df(rlist, function(m) range(m$results[[m$metric]]))
  y_range <- c(min(bounds[1, ]), max(bounds[2, ]))
  nrows <- ceiling(length(rlist) / 2)
  gg_list <-
    lapply(names(rlist), function(m) {
      mod <- rlist[[m]]
      # optimum is min or max depending on metric
      optimum <- if (mod$maximize) max else min
      best_metric <- round(optimum(mod$results[[mod$metric]]), 2)
      ggplot(mod) +
        ylim(y_range) +
        labs(title = m,
             caption = paste("Best ", mod$metric, ": ", best_metric))
    })
  gg <- cowplot::plot_grid(plotlist = gg_list)
  if (print)
    print(gg)
  return(invisible(gg))
}

summary.regression_list <- function(rlist) {
  # Data details: rows, features, outcome class/alg type
  # CV Details: n-folds, param depth
  # Names of algs
  # Best performing alg and hyperparameter values
  # Tables of hyperparamter values and performance
  names(m[[1]])
  m[[1]]$trainingData

}

print.regression_list <- function(rlist) {
  # Algs trained, depth
  # Best model: Alg, hyperparameters, metric
}

evaluate.regression_list <- function(rlist) {

  return()  # Best model
}
