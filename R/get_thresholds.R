#' Get class-separating thresholds for classification predictions
#'
#' @param x Either a predictions data frame (from \code{predict}) or a
#'   model_list (e.g. from \code{machine_learn}).
#' @param measures Character vector of performance metrics to calculate. The
#' returned data frame will have one column for each metric. Any
#'   of the non-scaler measure arguements to \code{ROCR::performance} should work.
#'   Defaults to all of the following: \itemize{
#'   \item{cost: Captures how bad all the errors are. You can adjust the relative costs
#'    of false alarms and missed detections by setting \code{cost.fp} or
#'    \code{cost.fn}}. At the default of equal costs, this is directly inversely
#'    proportional to accuracy.
#'   \item{acc: Accuracy}
#'   \item{tpr: True positive rate, aka sensitivity, aka recall}
#'   \item{tnr: True negative rate, aka specificity}
#'   \item{fpr: False positive rate, aka fallout}
#'   \item{fnr: False negative rate}
#'   \item{ppv: Positive predictive value, aka precision}
#'   \item{npv: Negative predictive value}
#'   }
#' @param cost.fp Cost of a false positive. Default = 1. Only affects cost.
#' @param cost.fn Cost of a false negative. Default = 1. Only affects cost.
#'
#' @return Tibble with rows for each possible threshold value
#' and columns for the thresholds and each value in \code{measures}. Rows are
#' sorted increasing by the first value in measures (cost by default).
#' @export
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#'
#' @examples
#' machine_learn(pima_diabetes[1:20, ], patient_id, outcome = diabetes,
#'               models = "rf", tune = FALSE) %>%
#'   get_thresholds()
get_thresholds <- function(x,
                           measures = c("cost", "acc", "tpr", "fnr", "tnr", "fpr", "ppv", "npv"),
                           cost.fp = 1, cost.fn = 1) {
  if (is.model_list(x))
    x <- predict(x)
  mi <- attr(x, "model_info")
  if (mi$type != "Classification")
    stop("get_thresholds only works for classification models. x looks like it's ", mi$type)
  target <- mi$target
  actual <- x[[target]]
  if (is.null(labels))
    stop("x doesn't have outcomes, so get_thresholds can't calculate performance metrics")
  preds <- x[[paste0("predicted_", target)]]
  # If positive class is in the actual vector, set the positive class explicitly:
  ordered_outcomes <-
    if (mi$positive_class %in% actual) {
    c(dplyr::setdiff(unique(actual), mi$positive_class), mi$positive_class)
    } else {
      NULL
    }
  # Create ROCR prediction object on which to calculate measures
  pred_obj <- ROCR::prediction(predictions = preds,
                               labels = actual,
                               label.ordering = ordered_outcomes)
  thresholds <- unlist(pred_obj@cutoffs)
  # Calculate measures and put them in a data frame with the thresholds
  out_df <-
    lapply(measures, function(x) {
      scores <- unlist(ROCR::performance(pred_obj, x, cost.fp = cost.fp, cost.fn = cost.fn)@y.values)
      if (length(scores) != length(thresholds))
        stop(x, ", which you supplied to `measures` doesn't seem to produce one ",
             "value per threshold. Check `?ROCR::performance`. ",
             "Here's what ", x, " produced:\n", scores)
      return(scores)
    }) %>%
    setNames(measures) %>%
    dplyr::bind_cols(threshold = thresholds, .)
  class(out_df) <- c("thresholds_df", class(out_df))
  return(out_df)
}

#' Plot threshold performance metrics
#'
#' @param x A \code{threshold_df} object from \code{\link{get_thresholds}} or a
#'  data frame with columns "threshold" and other columns to be plotted against thresholds
#' @param title Plot title. Default NULL produces no title
#' @param caption Plot caption. Default NULL produces no caption
#' @param font_size Relative size of all fonts in plot, default = 11
#' @param line_size Width of lines, default = 0.5
#' @param point_size Point size. Default is \code{NA} which suppresses points.
#' Set to a number to see where threholds are.
#' @param ncol Number of columns of facets.
#' @param print Print the plot? Default = TRUE
#' @param ... Unused
#'
#' @return A ggplot object, invisibly.
#' @export
#' @seealso \code{\link{get_thresholds}}
#'
#' @examples
#' m <- machine_learn(pima_diabetes[1:100, ], patient_id, outcome = diabetes,
#'                    models = "xgb", tune = FALSE, n_folds = 3)
#' get_thresholds(m) %>%
#'   plot()
#' thresh <- get_thresholds(m, measures = c("acc", "cost"), cost.fn = 3)
#' plot(thresh, point_size = .5, ncol = 1, caption = "Try setting cost.fn = 10") +
#'   geom_vline(xintercept = thresh$threshold[which.min(thresh$cost)],
#'              linetype = "dashed", color = "firebrick")
plot.thresholds_df <- function(x, title = NULL, caption = NULL, font_size = 11,
                               line_size = .5, point_size = NA, ncol = 2,
                               print = TRUE, ... ) {
  if ( !is.data.frame(x) || !"threshold" %in% names(x) )
    stop("x must be a data frame from get_thresholds, or at least look like one!")
  the_plot <-
    x %>%
    tidyr::gather(measure, value, -threshold) %>%
    ggplot(aes(x = threshold, y = value)) +
    facet_wrap(~ measure, ncol = ncol, scales = "free_y") +
    geom_point(size = point_size, na.rm = TRUE) +
    geom_line(size = line_size, na.rm = TRUE) +
    ylab(NULL) +
    labs(title = title, caption = caption) +
    theme_gray(base_size = font_size)

  if (print)
    print(the_plot)
  return(invisible(the_plot))
}
