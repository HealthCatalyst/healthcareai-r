#' print method for model_list
#'
#' @param x model_list
#'
#' @export
#' @noRd
print.model_list <- function(x, ...) {
  if (length(x)) {
    x <- change_metric_names(x)
    rinfo <- extract_model_info(x)
    out <- paste0(
      "Algorithms Trained: ", list_variables(rinfo$algs),
      "\nModel Name: ", rinfo$model_name,
      "\nTarget: ", rinfo$target,
      "\nClass: ", rinfo$m_class,
      "\nPerformance Metric: ", rinfo$metric,
      "\nNumber of Observations: ", rinfo$ddim[1],
      "\nNumber of Features: ", rinfo$ddim[2] - 1L,
      "\nModels Trained: ", rinfo$timestamp
    )
    out <- paste(
      out,
      if (rinfo$tuned) {
        paste0("\n\nModels tuned via ", x[[1]]$control$number, "-fold cross validation ",
               "over ", nrow(x[[1]]$results), " combinations of hyperparameter values.",
               "\nBest model: ", rinfo$best_model_name,
               "\n", format_performance(attr(x, "performance")),
               "\nOptimal hyperparameter values:", "\n  ", format_tune(rinfo$best_model_tune)
        )
      } else {
        paste0("\n\nModels have not been tuned. Performance estimated via ",
               x[[1]]$control$number, "-fold cross validation at fixed hyperparameter values.",
               "\nBest model: ", rinfo$best_model_name,
               "\n", format_performance(attr(x, "performance")),
               "\nUser-selected hyperparameter values:", "\n  ", format_tune(rinfo$best_model_tune)
        )
      }
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
#'
#' @export
#' @noRd
summary.model_list <- function(object, ...) {
  if (!length(object))
    stop("object is empty.")
  object <- change_metric_names(object)
  rinfo <- extract_model_info(object)
  out <-
    if (rinfo$tuned) {
      paste0("Models trained: ", rinfo$timestamp,
             "\n\nModels tuned via ", object[[1]]$control$number, "-fold cross validation ",
             "over ", nrow(object[[1]]$results), " combinations of hyperparameter values.",
             "\nBest performance: ", format_performance(attr(object, "performance")),
             "\nBy ", rinfo$best_model_name, " with hyperparameters:\n  ",
             format_tune(rinfo$best_model_tune))
    } else {
      paste0("Models trained: ", rinfo$timestamp,
             "\n\nModels have not been tuned. Performance estimated via ",
             object[[1]]$control$number, "-fold cross validation at fixed hyperparameter values.",
             "\nBest algorithm: ", rinfo$best_model_name, " with ",
             format_performance(attr(object, "performance")))
    }
  cat(out)
  cat("\n\nOut-of-fold performance of all trained models:\n\n")
  perf <- lapply(object, function(xx) {
    ord <- order(xx$results[[rinfo$metric]])
    if (object[[1]]$maximize) ord <- rev(ord)
    tibble::as_tibble(xx$results[ord, ])
  })
  names(perf) <- rinfo$algs
  print(perf)
  return(invisible(perf))
}

#' Plot performance of models
#'
#' @param x modellist object as returned by \code{\link{tune_models}} or
#'   \code{\link{machine_learn}}
#' @param font_size Relative size of all fonts in plot, default = 11
#' @param point_size Size of dots, default = 3
#' @param print If TRUE (default) plot is printed
#' @param ... Unused
#'
#' @return Plot of model performance as a function of algorithm and
#'   hyperparameter values tuned over. Generally called for the side effect of
#'   printing a plot, but the plot is also invisibly returned. The
#'   best-performing model within each algorithm will be plotted as a triangle.
#'
#' @importFrom cowplot plot_grid
#' @importFrom purrr map_df
#' @export
#' @examples
#' models <- tune_models(mtcars, mpg, models = "glm")
#' plot(models)
plot.model_list <- function(x, font_size = 11, point_size = 1,
                            print = TRUE, ...) {
  if (!length(x))
    stop("x is empty.")
  if (!inherits(x, "model_list"))
    stop("x is class ", class(x)[1], ", but needs to be model_list")
  if (!attr(x, "tuned"))
    message("Use `tune_models()` or `machine_learn(... , tune = TRUE)` to tune hyperparameters,",
            " or use `predict(models) %>% plot()` to plot predictions on training data.")
  x <- change_metric_names(x)
  params <- purrr::map(x, ~ as.character(.x$modelInfo$parameters$parameter))
  bounds <- purrr::map_df(x, function(m) range(m$results[[m$metric]]))
  y_range <- c(min(bounds[1, ]), max(bounds[2, ]))
  gg_list <-
    # Loop over algorithms
    lapply(x, function(mod) {
      # optimum is min or max depending on metric
      optimum <- if (mod$maximize) max else min
      mod$results$id <- as.character(sample(nrow(mod$results)))
      mod$results$best <- mod$results[[mod$metric]] == optimum(mod$results[[mod$metric]])
      hps <- as.character(mod$modelInfo$parameters$parameter)
      plots <-
        # Loop over hyperparameters
        purrr::map(hps, ~ {
          to_plot <- mod$results[, which(names(mod$results) %in% c(.x, mod$metric, "best", "id"))]
          # Add column with a unique identifier for each row to color by
          if (!is.numeric(to_plot[[.x]]))
            to_plot[[.x]] <- reorder(to_plot[[.x]], to_plot[[mod$metric]], FUN = optimum)
          p <-
            ggplot(to_plot, aes_string(x = .x, y = mod$metric,
                                       color = "id", shape = "best")) +
            geom_point(size = point_size) +
            coord_flip() +
            scale_y_continuous(limits = y_range) +
            scale_color_discrete(guide = FALSE) +
            scale_shape_manual(values = c("TRUE" = 17, "FALSE" = 16), guide = FALSE) +
            xlab(NULL) +
            labs(title = .x) +
            theme_gray(base_size = font_size)
          if (.x == "lambda" && mod$modelInfo$label == "glmnet")
            p <- p + scale_x_log10()
          p <-
            if (.x != hps[length(hps)]) {
              p + theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank())
            } else {
              p + theme(axis.title.x = element_text(face = "bold"))
            }
          return(p)
        })
      title <-
        cowplot::ggdraw() +
        cowplot::draw_label(mod$modelInfo$label, fontface = "bold")
      ncols <- if (mod$modelInfo$label == "eXtreme Gradient Boosting") 2 else 1
      cowplot::plot_grid(plotlist = plots, ncol = ncols, align = "v") %>%
        plot_grid(title, ., ncol = 1, rel_heights = c(0.1, 1.9))
    })
  gg <- cowplot::plot_grid(plotlist = gg_list, nrow = 1)
  if (print)
    print(gg)
  return(invisible(gg))
}

#' @export
`[.model_list` <- function(x, i) {
  attrs <- attributes(x)
  if (is.logical(i)) {
    i <- which(i)
  } else if (is.character(i)) {
    i <- which(names(x) %in% i)
  }
  # Training data is held in first model only; move it there if it's not staying
  if (!1 %in% i)
    x[[min(i)]]$trainingData <- x[[1]]$trainingData
  # Rebuild the model_list, keeping the old timestamp
  x <-
    as.model_list(listed_models = .subset(x, i),
                  target = attrs$target,
                  tuned = attrs$tuned,
                  recipe = attrs$recipe,
                  positive_class = attrs$positive_class,
                  original_data_str = attrs$original_data_str) %>%
    structure(timestamp = attrs$timestamp)
  return(x)
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
  best_metrics <- purrr::map_dbl(x, ~ optimum(.x$results[[metric]], na.rm = TRUE))
  best_model <- which(best_metrics == optimum(best_metrics))[1] # 1 in case tie
  algs <- purrr::map_chr(x, ~ .x$modelInfo$label)
  m_class <- x[[1]]$modelType
  target <- attr(x, "target")
  ddim <- dim(x[[1]]$trainingData)
  best_model_name <- algs[[best_model]]
  best_model_perf <- best_metrics[[best_model]]
  best_model_tune <-
    x[[best_model]]$bestTune
  positive_class <- attr(x, "positive_class")
  from_rds <- attr(x, "loaded_from_rds")
  if (is.null(from_rds))
    from_rds <- "trained_in_memory"
  list(
    model_name = attr(x, "model_name"),
    m_class = m_class,
    algs = algs,
    target = target,
    positive_class = positive_class,
    metric = metric,
    best_model_name = best_model_name,
    best_model_perf = best_model_perf,
    best_model_tune = best_model_tune,
    ddim = ddim,
    tuned = attr(x, "tuned"),
    timestamp = attr(x, "timestamp"),
    from_rds = from_rds
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
    purrr::map(~ {
      if (is.numeric(.x))
        .x <- signif(.x, 2)
      as.character(.x)
    }) %>%
    paste(names(.), ., sep = " = ", collapse = "\n  ")
}

format_performance <- function(perf) {
  signif(perf, 2) %>%
    paste(names(.), ., sep = " = ", collapse = ", ")
}

#' Type checks
#' @export
#' @param x Object
#' @rdname is.model_list
#' @return Logical
is.model_list <- function(x) "model_list" %in% class(x)

#' @export
#' @rdname is.model_list
is.classification_list <- function(x) "classification_list" %in% class(x)

#' @export
#' @rdname is.model_list
is.regression_list <- function(x) "regression_list" %in% class(x)
