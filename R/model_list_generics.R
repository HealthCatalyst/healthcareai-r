#' print method for model_list
#'
#' @param x model_list
#'
#' @export
#' @noRd
print.model_list <- function(x, ...) {
  if (length(x)) {
    x <- change_pr_metric(x)
    rinfo <- extract_model_info(x)
    out <- paste0(
      "Target: ", rinfo$target,
      "\nClass: ", rinfo$m_class,
      "\nAlgorithms Tuned: ", paste(rinfo$algs, collapse = ", "),
      "\nPerformance Metric: ", rinfo$metric,
      "\nNumber of Observations: ", rinfo$ddim[1],
      "\nNumber of Features: ", rinfo$ddim[2] - 1L,

      "\n\nBest model: ", rinfo$best_model_name,
      "\n", rinfo$metric, " = ", round(rinfo$best_model_perf, 2),
      "\nHyperparameter values:", "\n  ", format_tune(rinfo$best_model_tune)
    )
  } else {
    out <- paste("Empty", class(x)[1], "object.")
  }
  cat(out, "\n")
  return(invisible(x))
}

#' summary method for model_list
#'
#' @param x model_list
#' @return list of tuning performance data frames, invisibly
#' @importFrom dplyr %>%
#'
#' @export
#' @noRd
summary.model_list <- function(object, ...) {
  if (!length(object))
    stop("object is empty.")
  object <- change_pr_metric(object)
  rinfo <- extract_model_info(object)
  out <- paste0("Best performance: ", rinfo$metric, " = ",
                round(rinfo$best_model_perf, 2), "\n",
                rinfo$best_model_name, " with hyperparameters:\n  ",
                format_tune(rinfo$best_model_tune))
  cat(out)
  cat("\n\nOut-of-fold performance of all trained models:\n\n")
  perf <- lapply(object, function(xx) {
    ord <- order(xx$results[[rinfo$metric]])
    if (object[[1]]$maximize) ord <- rev(ord)
    structure(xx$results[ord, ], row.names = seq_len(nrow(xx$results)))
  })
  names(perf) <- rinfo$algs
  print(perf)
  return(invisible(perf))
}

#' Plot performance of models
#'
#' @param x modellist object as returned by \code{\link{tune_models}} or
#'   \code{\link{machine_learn}}
#' @param print If TRUE (default) plot is printed
#' @param ... generic compatability
#'
#' @return Plot of model performance as a function of algorithm and
#'   hyperparameter values tuned over. Generally called for the side effect of
#'   printing a plot, but the plot is also invisibly returned.
#'
#' @importFrom cowplot plot_grid
#' @importFrom purrr map_df
#' @export
#' @examples
#' plot(tune_models(mtcars, mpg))
plot.model_list <- function(x, print = TRUE, ...) {
  if (!length(x))
    stop("x is empty.")
  if (!inherits(x, "model_list"))
    stop("x is class ", class(x)[1],
         ", but needs to be model_list")
  bounds <- purrr::map_df(x, function(m) range(m$results[[m$metric]]))
  y_range <- c(min(bounds[1, ]), max(bounds[2, ]))
  nrows <- ceiling(length(x) / 2)
  gg_list <-
    lapply(x, function(mod) {
      # optimum is min or max depending on metric
      optimum <- if (mod$maximize) max else min
      best_metric <- round(optimum(mod$results[[mod$metric]]), 2)
      ggplot(mod) +
        ylim(y_range) +
        labs(title = mod$modelInfo$label,
             caption = paste("Best ", mod$metric, ": ", best_metric))
    })
  gg <- cowplot::plot_grid(plotlist = gg_list, nrow = nrows)
  if (print)
    print(gg)
  return(invisible(gg))
}

if (FALSE) {
  # This is tricky because finalModel is a ranger (or whatever) class object,
  # not a train object.
  evaluate.model_list <- function(x) {
    f <- if (x[[1]]$maximize) max else min
    each_best <- purrr::map_dbl(x, ~ f(.x$results[[.x$metric]]))
    which_best <- which(f(each_best) == each_best)[1]
    message("Returning the best model, a ", names(which_best))
    out <- x[[which_best]]$finalModel
    return(out)
  }
}

#' Get info from a model_list
#'
#' @param x model_list
#' @importFrom purrr map_chr
#' @return list of statistics
#' @noRd
extract_model_info <- function(x) {
  # optimum is min or max depending on metric
  optimum <- if (x[[1]]$maximize) max else min
  metric <- x[[1]]$metric
  best_metrics <- purrr::map_dbl(x, ~ optimum(.x$results[[metric]]))
  best_model <- which(best_metrics == optimum(best_metrics))[1] # 1 in case tie
  algs <- purrr::map_chr(x, ~ .x$modelInfo$label)
  m_class <- x[[1]]$modelType
  target <- attr(x, "target")
  ddim <- dim(x[[1]]$trainingData)
  best_model_name <- algs[[best_model]]
  best_model_perf <- best_metrics[[best_model]]
  best_model_tune <-
    x[[best_model]]$bestTune
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

#' Format extract_model_info()$best_model_tune for printing
#'
#' @param best_tune character vector
#' @importFrom purrr map_chr
#' @return character vector for printing
#' @noRd
format_tune <- function(best_tune) {
  best_tune %>%
    purrr::map_chr(as.character) %>%
    paste(names(.), ., sep = " = ", collapse = "\n  ")
}

#' Class check
#' @param x object
#' @return logical
#' @export
is.model_list <- function(x) "model_list" %in% class(x)
#' Class check
#' @param x object
#' @return logical
#' @export
is.classification_list <- function(x) "classification_list" %in% class(x)
#' Class check
#' @param x object
#' @return logical
#' @export
is.regression_list <- function(x) "regression_list" %in% class(x)

#' Modify model object if PR. Otherwise, return as is.
#' @param m model_list
#' @return model_list
#' @noRd
change_pr_metric <- function(m) {
  if (m[[1]]$metric == "AUC") { # PR was used
    m <- purrr::map(m, function(x) {
      x$metric <- "PR"
      names(x$results)[names(x$results) == "AUC"] <- "PR"
      return(x)
    })
    return(m)
  } else {
    return(m)
  }
}
