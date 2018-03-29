#' Plot model predictions vs observed outcomes
#'
#' @param x data frame as returned `predict.model_list`
#' @param outcomes Vector of outcomes if not present in x
#' @param print Logical, if TRUE (default) the plot is printed on the current
#'   graphics device. The plot is always (silently) returned.
#' @param ... Parameters to pass to plot_regression_predictions or
#'   plot_classification_predictions
#'
#' @return A ggplot object
#' @export
#'
#' @details The following arguments can be provided to customize the plot: For
#'   regression: title, point_size, point_alpha, font_size. For
#'   classification: title, fill_colors, fill_alpha, curve_flex, font_size. For
#'   details on how to use them, see \code{\link{plot_regression_predictions}}
#'   or \code{\link{plot_classification_predictions}}.
#'
#' @examples
#' models <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = plasma_glucose)
#' predictions <- predict(models)
#' plot(predictions)
#' plot(predictions, title = "This model's predictions regress to the mean",
#'      point_size = 3, point_alpha = .7, font_size = 14)
plot.hcai_predicted_df <- function(x, outcomes = NULL, print = TRUE, ...) {

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

  # Print and return
  if (print)
    print(the_plot)
  return(invisible(the_plot))
}

#' Plot predictions from a regression model
#'
#' @param x hcai_predicted_df from plot.hcai_predicted_df
#' @param title Character: Plot title
#' @param point_size Number: Point size, relative to 1
#' @param point_alpha Number in [0, 1] giving point opacity
#' @param font_size Number: Relative size of all font in plot, default = 11
#' @param target outcome column name
#'
#' @return ggplot object
#'
#' @details This function is not meant to be called directly. Use
#'   \code{plot(predictions)} instead. You can pass arguments to this function
#'   (e.g. a plot title) through \code{plot}.
plot_regression_predictions <- function(x,
                                        title = NULL,
                                        point_size = 1,
                                        point_alpha = 1,
                                        font_size = 11,
                                        target) {
  preds <- paste0("predicted_", target)
  limits <- range(c(x[[target]]), x[[preds]])
  p <-
    ggplot(x, aes_string(x = target, y = preds)) +
    geom_abline(linetype = "dashed", alpha = .6)  +
    geom_point(size = point_size, alpha = point_alpha) +
    scale_x_continuous(name = paste("Actual ", target), limits = limits) +
    scale_y_continuous(name = paste("Predicted ", target), limits = limits) +
    ggtitle(title) +
    theme_gray(base_size = font_size)
  return(p)
}

#' Plot predictions from a classification model
#'
#' @param x hcai_predicted_df from plot.hcai_predicted_df
#' @param title Character: Plot title, default NULL produces no title.
#' @param fill_colors Length-2 character vector: colors to fill density
#'   curves. Default is c("firebrick", "steelblue"). If named, names must match
#'   \code{unique(x[[target]])}, in any order.
#' @param fill_alpha Number in [0, 1] giving opacity of fill colors.
#' @param curve_flex Numeric. Kernal adjustment for density curves. Default is 1.
#'   Less than 1 makes curves more flexible, analogous to smaller bins in a
#'   histogram; greater than 1 makes curves more rigid.
#' @param font_size Number: Relative size of all font in plot, default = 11
#' @param target outcome column name
#'
#' @return ggplot object
#'
#' @details This function is not meant to be called directly. Use
#'   \code{plot(predictions)} instead. You can pass arguments to this function
#'   (e.g. a plot title) through \code{plot}.
plot_classification_predictions <- function(x,
                                            title = NULL,
                                            fill_colors = c("firebrick", "steelblue"),
                                            fill_alpha = .7,
                                            curve_flex = 1,
                                            font_size = 11,
                                            target) {
  preds <- paste0("predicted_", target)
  p <-
    ggplot(x, aes_string(x = preds, fill = target)) +
    geom_density(alpha = fill_alpha, adjust = curve_flex) +
    scale_x_continuous(name = paste("Predicted Probability of", target),
                       limits = c(0, 1)) +
    scale_fill_manual(name = paste0("Actual\n", target), values = fill_colors) +
    ggtitle(title) +
    theme_gray(base_size = font_size)
  return(p)
}
