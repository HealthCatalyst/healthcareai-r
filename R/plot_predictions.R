#' Plot model predictions vs observed outcomes
#'
#' @param x data frame as returned `predict.model_list`
#' @param caption Put model performance in plot caption? TRUE (default) prints
#'   all available metrics, FALSE prints nothing. Can also provide metric name
#'   (e.g. "RMSE"), in which case the caption will include only that metric.
#' @param title Character: Plot title, default NULL produces no title.
#' @param font_size Number: Relative size of all font in plot, default = 11
#' @param fixed_aspect Logical: If TRUE (default for regression only), units of
#'   the x- and y-axis will have the same spacing.
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
#' # Some regression examples
#' models <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = plasma_glucose,
#'                         models = "rf", tune = FALSE)
#' predictions <- predict(models)
#' plot(predictions)
#' plot(predictions, caption = "Rsquared",
#'      title = "This model's predictions regress to the mean",
#'      point_size = 3, point_alpha = .7, font_size = 9)
#' p <- plot(predictions, print = FALSE)
#' p + theme_classic()
#'
#' # A classification example with risk groups
#' class_models <- machine_learn(pima_diabetes, patient_id, outcome = diabetes,
#'                               models = "xgb", tune = FALSE)
#' predict(class_models,
#'         risk_groups = c("v low", "low", "medium", "high", "very high")) %>%
#'   plot()
plot.predicted_df <- function(x,
                              caption = TRUE,
                              title = NULL,
                              font_size = 11,
                              outcomes = NULL,
                              fixed_aspect = attr(x, "model_info")$type == "Regression",
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
  } else if (mi$type == "Multiclass") {
    the_plot <- plot_multiclass_predictions(x, target = target, ...)
  } else {
    stop("Plotting predictions of model type ", mi$type, " is not currently supported.")
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
  x_angle <- the_plot$theme$axis.text.x$angle #nolint
  the_plot <-
    the_plot +
    labs(caption = cap, title = title) +
    theme_gray(base_size = font_size)
  if (!is.null(fixed_aspect) && fixed_aspect)
    the_plot <- the_plot + coord_fixed()
  if (is.numeric(x_angle))
    the_plot <- the_plot + theme(axis.text.x = element_text(angle = x_angle, hjust = 1))
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

#' @param fill_colors Length-2 character vector: colors to fill density curves.
#'   Default is c("firebrick", "steelblue"). If named, names must match
#'   \code{unique(x[[target]])}, in any order.
#' @param fill_alpha Number in [0, 1] giving opacity of fill colors.
#' @param curve_flex Numeric. Kernal adjustment for density curves. Default is
#'   1. Less than 1 makes curves more flexible, analogous to smaller bins in a
#'   histogram; greater than 1 makes curves more rigid.
#' @param add_labels If TRUE (default) and a predicted_group column was added to
#'   predictions by specifying \code{risk_groups} or \code{outcome_groups} in
#'   \code{link{predict.model_list}}, labels specifying groups are added to the
#'   plot.
#'
#' @rdname plot.predicted_df
plot_classification_predictions <- function(x,
                                            fill_colors = c("firebrick", "steelblue"),
                                            fill_alpha = .7,
                                            curve_flex = 1,
                                            add_labels = TRUE,
                                            target) {
  preds <- paste0("predicted_", target)
  p <-
    ggplot() +
    geom_density(data = x,
                 mapping = aes_string(x = preds, fill = target),
                 alpha = fill_alpha, adjust = curve_flex) +
    scale_x_continuous(name = paste("Predicted Probability of", target),
                       limits = c(0, 1)) +
    scale_fill_manual(name = paste0("Actual\n", target), values = fill_colors,
                      guide = guide_legend(reverse = TRUE))

  if (add_labels && "predicted_group" %in% names(x)) {
    group_attrs <- attributes(x$predicted_group)
    max_height <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2] * .98
    if (group_attrs$group_type == "outcome") {
      p <- p +
        geom_vline(xintercept = group_attrs$cutpoints, linetype = "dashed") +
        geom_label(aes(label = paste("Predicted", group_attrs$levels),
                       x = group_attrs$cutpoints, y = max_height),
                   hjust = c(1.1, -.1), vjust = 1)
    } else if (group_attrs$group_type == "risk") {
      ct_pts <- group_attrs$cutpoints[-c(1, length(group_attrs$cutpoints))]
      labels <- group_attrs$levels
      v_just <- -1 + 1.1 * seq_len(ceiling(length(labels) / 2))
      half2 <- if (length(labels) %% 2) v_just[-length(v_just)] else v_just
      v_just <- c(v_just, rev(half2))
      p <- p +
        geom_vline(xintercept = ct_pts, linetype = "dashed") +
        geom_label(aes(label = labels,
                       x = c(0, ct_pts), y = max_height), hjust = -.1, vjust = v_just)
    }
  }

  return(p)
}

#' @param conf_colors Length-2 character vector: colors to fill density curves.
#'   Default is c("black", "steelblue").
#' @param text_color Character: color to write percent correct.
#'   Default is "yellow".
#' @param text_size Numeric or logical: size of percent correct text. Defaults to
#' 3, a readable size. Greater than 20 classes might need smaller text. Text can be
#'  turned off by setting to FALSE.
#' @param text_angle Numeric or logical: angle to rotate x axis text. Defaults to
#' 60 degrees. Setting to FALSE will turn text horizontal.
#' @param diag_color Character: color to highlight main diagonal. These are
#'   correct predictions. Default is "red".
#' @rdname plot.predicted_df
plot_multiclass_predictions <- function(x,
                                        conf_colors = c("black", "steelblue"),
                                        text_color = "yellow",
                                        text_size = 3,
                                        text_angle = 60,
                                        diag_color = "red",
                                        target) {
  preds <- paste0("predicted_", target)
  # Only show actuals that are in the data.
  x[[target]] <- factor(x[[target]])
  if (!any(levels(x[[target]]) %in% levels(x[[preds]])))
    stop("Something went wrong, the predictions don't look like the same data ",
         "as the true values. Predictions look like: '",
         list_variables(levels(x[[preds]])[1:2]), "' and outcomes look like: '",
         list_variables(levels(x[[target]])[1:2]), "'")

  # Compute frequency of actual categories
  actual <- as.data.frame(table(x[[target]])) %>%
    rename(!!target := Var1,
           actual_freq = Freq) %>%
    as_tibble()
  # Build confusion matrix
  confusion <- as.data.frame(table(x[[target]], x[[preds]])) %>%
    rename(!!target := Var1,
           !!preds := Var2,
           freq = Freq) %>%
    as_tibble()

  # Calculate percentage of test cases based on actual frequency
  confusion <- inner_join(confusion, actual, by = target) %>%
    mutate(!!target := as.factor(!!rlang::sym(target)),
           !!preds := as.factor(!!rlang::sym(preds)),
           percent = freq / actual_freq * 100,
           diag = as.character(!!rlang::sym(target)) ==
             as.character(!!rlang::sym(preds)))
  # Reorder levels
  confusion[[preds]] <- factor(confusion[[preds]], levels = rev(levels(confusion[[preds]])))
  # Plot
  # Draw tiles, fill color
  p <- confusion %>%
    ggplot(aes_string(x = target, y = preds)) +
    geom_tile(mapping = aes(fill = percent),
              color = "black",
              size = 0.1) +
    xlab(target) +
    ylab(preds) +
    coord_fixed()
  # Write percentage in each tile
  if (!is.numeric(text_size) & !is.logical(text_size))
    stop("text_size must be logical or numeric")
  if (isTRUE(text_size))
    text_size <- 3
  if (is.numeric(text_size))
    p <- p +
    geom_text(aes(label = sprintf("%.1f", percent)), size = text_size, color = text_color)

  p <- p +
    scale_fill_gradient(name = "% Matched",
                        low = conf_colors[1],
                        high = conf_colors[2])
  # Highlight diagonals
  p <- p +
    geom_tile(data = filter(confusion, diag),
              color = diag_color, size = 0.3, fill = "black", alpha = 0)

  # Rotate labels
  if (!is.numeric(text_angle) & !is.logical(text_angle))
    stop("text_angle must be logical or numeric")
  if (isTRUE(text_angle))
    text_size <- 3
  if (is.numeric(text_angle))
    p <- p + theme(axis.text.x = element_text(angle = text_angle, hjust = 1))
  return(p)
}
