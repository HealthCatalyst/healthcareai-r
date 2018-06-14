#' Plot model predictions vs observed outcomes
#'
#' @param x data frame as returned `predict.model_list`
#' @param caption Put model performance in plot caption? TRUE (default) prints
#'   all available metrics, FALSE prints nothing. Can also provide metric name
#'   (e.g. "RMSE"), in which case the caption will include only that metric.
#' @param title Character: Plot title, default NULL produces no title.
#' @param font_size Number: Relative size of all font in plot, default = 11
#' @param outcomes Vector of outcomes if not present in x
#' @param print Logical, if TRUE (default) the plot is printed on the current
#'   graphics device. The plot is always (silently) returned.
#' @param ... Parameters specific to plot_regression_predictions or
#'   plot_classification_predictions; listed below. These must be named.
#'
#' @return A ggplot object
#' @export
#'
#' @details Note that a ggplot object is returned, so you can do additional
#'   customization of the plot. See the third example.
#'
#' @examples
#' models <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = plasma_glucose,
#'                         models = "rf", tune = FALSE)
#' predictions <- predict(models)
#' plot(predictions)
#' plot(predictions, caption = "Rsquared",
#'      title = "This model's predictions regress to the mean",
#'      point_size = 3, point_alpha = .7, font_size = 14)
#' p <- plot(predictions, print = FALSE)
#' p + coord_fixed(ratio = 1) + theme_classic()
plot.predicted_df <- function(x,
                              caption = TRUE,
                              title = NULL,
                              font_size = 11,
                              outcomes = NULL,
                              print = TRUE,
                              ...) {
  # Checks, and put outcomes in x if necessary
  mi <- attr(x, "model_info")
  target <- mi$target
  if (!is.null(outcomes)) {
    #  outcomes provided
    if (target %in% names(x))
      warning(target, " is present in `x`, but you passed values to `outcomes`. ",
              "The values in x will be ignored.")
    if (length(outcomes) != nrow(x))
      stop("There are ", nrow(x), " rows in `x` but ", length(outcomes), " values in `outcomes`. ",
           "These must be the same length.")
    x[[target]] <- outcomes
  } else {
    if (!target %in% names(x))
      stop("Outcome column \"", target, "\" is not present in `x`, so you must pass a vector of outcomes to `outcomes`.")
  }

  # Check outcome type and dispatch the plotting function for the model type
  if (mi$type == "Regression") {
    if (!is.numeric(x[[target]]))
      stop("x comes from a regression model, but `outcomes` is class ", class(outcomes))
    the_plot <- plot_regression_predictions(x, target = target, ...)
  } else if (mi$type == "Classification") {
    distinct_outcomes <- length(unique(x[[target]]))
    if (distinct_outcomes > 2)
      stop("x comes from a classification model, but there are ",
           distinct_outcomes, " distinct outcomes in `outcomes`.")
    the_plot <- plot_classification_predictions(x, target = target, ...)
  } else {
    stop("Plotting predictions of model type ", mi$type, "is not currently supported.")
  }

  cap <-
    if (isTRUE(caption)) {
      format_performance(evaluate(x))
    } else if (caption != FALSE) {
      perf <- evaluate(x)
      if (!caption %in% names(perf))
        stop(caption, " not a performance metric for this model. Available metrics: ",
             list_variables(names(perf)))
      format_performance(perf[caption])
    } else {
      NULL
    }
  the_plot <-
    the_plot +
    labs(caption = cap, title = title) +
    theme_gray(base_size = font_size)
  # Print and return
  if (print)
    print(the_plot)
  return(invisible(the_plot))
}

#' @param point_size Number: Point size, relative to 1
#' @param point_alpha Number in [0, 1] giving point opacity
#' @param target Not meant to be set by user. outcome column name
#'
#' @rdname plot.predicted_df
plot_regression_predictions <- function(x,
                                        point_size = 1,
                                        point_alpha = 1,
                                        target) {
  preds <- paste0("predicted_", target)
  limits <- range(c(x[[target]]), x[[preds]])
  p <-
    ggplot(x, aes_string(x = target, y = preds)) +
    geom_abline(linetype = "dashed", alpha = .6)  +
    geom_point(size = point_size, alpha = point_alpha) +
    scale_x_continuous(name = paste("Actual ", target), limits = limits) +
    scale_y_continuous(name = paste("Predicted ", target), limits = limits)
  return(p)
}

#' @param fill_colors Length-2 character vector: colors to fill density
#'   curves. Default is c("firebrick", "steelblue"). If named, names must match
#'   \code{unique(x[[target]])}, in any order.
#' @param fill_alpha Number in [0, 1] giving opacity of fill colors.
#' @param curve_flex Numeric. Kernal adjustment for density curves. Default is 1.
#'   Less than 1 makes curves more flexible, analogous to smaller bins in a
#'   histogram; greater than 1 makes curves more rigid.
#'
#' @rdname plot.predicted_df
plot_classification_predictions <- function(x,
                                            fill_colors = c("firebrick", "steelblue"),
                                            fill_alpha = .7,
                                            curve_flex = 1,
                                            target) {
  preds <- paste0("predicted_", target)
  p <-
    ggplot(x, aes_string(x = preds, fill = target)) +
    geom_density(alpha = fill_alpha, adjust = curve_flex) +
    scale_x_continuous(name = paste("Predicted Probability of", target),
                       limits = c(0, 1)) +
    scale_fill_manual(name = paste0("Actual\n", target), values = fill_colors)
  return(p)
}
