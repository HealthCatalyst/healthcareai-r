#' Get class-separating thresholds for classification predictions
#'
#' @param x Either a predictions data frame (from \code{predict}) or a
#'   model_list (e.g. from \code{machine_learn}).
#' @param optimize Optional. If provided, one of the entries in \code{measures}. A logical
#' column named "optimal" will be added with one TRUE entry corresponding to
#' the threshold that optimizes this measure.
#' @param measures Character vector of performance metrics to calculate, or "all",
#' which is equivalent to using all of the following measures. The
#' returned data frame will have one column for each metric. \itemize{
#'   \item{cost: Captures how bad all the errors are. You can adjust the relative costs
#'    of false alarms and missed detections by setting \code{cost_fp} or
#'    \code{cost_fn}}. At the default of equal costs, this is directly inversely
#'    proportional to accuracy.
#'   \item{acc: Accuracy}
#'   \item{tpr: True positive rate, aka sensitivity, aka recall}
#'   \item{tnr: True negative rate, aka specificity}
#'   \item{fpr: False positive rate, aka fallout}
#'   \item{fnr: False negative rate}
#'   \item{ppv: Positive predictive value, aka precision}
#'   \item{npv: Negative predictive value}
#'   }
#' @param cost_fp Cost of a false positive. Default = 1. Only affects cost.
#' @param cost_fn Cost of a false negative. Default = 1. Only affects cost.
#'
#' @return Tibble with rows for each possible threshold
#' and columns for the thresholds and each value in \code{measures}.
#' @export
#' @importFrom ROCR prediction
#' @importFrom ROCR performance
#'
#' @description healthcareai gives you predicted probabilities for classification
#' problems, but sometimes you need to convert probabilities into predicted
#' classes. That requires choosing a threshold, where probabilities above the
#' threshold are predicted as the positive class and probabilities below the
#' threshold are predicted as the negative class. This function helps you do that
#' by calculating a bunch of model-performance metrics at every possible
#' threshold.
#'
#' "cost" is an especially useful measure as it allows you to weight how bad a
#' false alarm is relative to a missed detection. E.g. if for your use case
#' a missed detection is five times as bad as a false alarm (another way to say
#' that is that you're willing to allow five false positives for every one
#' false negative), set \code{cost_fn = 5} and use the threshold that minimizes
#' cost (see \code{examples}).
#'
#' We recommend plotting the thresholds with their performance measures to
#' see how optimizing for one measure affects performance on other measures.
#' See \code{\link{plot.thresholds_df}} for how to do this.
#'
#' @examples
#' library(dplyr)
#' models <- machine_learn(pima_diabetes[1:15, ], patient_id, outcome = diabetes,
#'                         models = "xgb", tune = FALSE)
#' get_thresholds(models)
#'
#' # Identify the threshold that maximizes accuracy:
#' get_thresholds(models, optimize = "acc")
#'
#' # Assert that one missed detection is as bad as five false alarms and
#' # filter to the threshold that minimizes "cost" based on that assertion:
#' get_thresholds(models, optimize = "cost", cost_fn = 5) %>%
#'   filter(optimal)
#'
#' # Use that threshold to make class predictions
#' (class_preds <- predict(models, outcome_groups = 5))
#' attr(class_preds$predicted_group, "cutpoints")
#'
#' # Plot performance on all measures across threshold values
#' get_thresholds(models) %>%
#'   plot()
#'
#' # If a measure is provided to optimize, the best threshold will be highlighted in plots
#' get_thresholds(models, optimize = "acc") %>%
#'   plot()
#'
#' ## Transform probability predictions into classes based on an optimal threshold ##
#' # Pull the threshold that minimizes cost
#' optimal_threshold <-
#'   get_thresholds(models, optimize = "cost") %>%
#'   filter(optimal) %>%
#'   pull(threshold)
#'
#' # Add a Y/N column to predictions based on whether the predicted probability
#' # is greater than the threshold
#' class_predictions <-
#'   predict(models) %>%
#'   mutate(predicted_class_diabetes = case_when(
#'     predicted_diabetes > optimal_threshold ~ "Y",
#'     predicted_diabetes <= optimal_threshold ~ "N"
#'   ))
#'
#' class_predictions %>%
#'   select_at(vars(ends_with("diabetes"))) %>%
#'   arrange(predicted_diabetes)
#'
#' # Examine the expected volume of false-and-true negatives-and-positive
#' table(Actual = class_predictions$diabetes,
#'       Predicted = class_predictions$predicted_class_diabetes)
get_thresholds <- function(x, optimize = NULL, measures = "all",
                           cost_fp = 1, cost_fn = 1) {
  measures <- get_measures(measures)
  if (is.model_list(x))
    x <- predict(x)
  mi <- attr(x, "model_info")
  if (mi$type != "Classification")
    stop("get_thresholds only works for classification models. x looks like it's ", mi$type)
  target <- mi$target
  actual <- x[[target]]
  if (is.null(actual))
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
    lapply(names(measures), function(x) {
      scores <- unlist(
        ROCR::performance(pred_obj, x, cost.fp = cost_fp, cost.fn = cost_fn)@y.values)
      if (length(scores) != length(thresholds))
        stop(x, ", which you supplied to `measures` doesn't seem to produce one ",
             "value per threshold. Check `?ROCR::performance`. ",
             "Here's what ", x, " produced:\n", scores)
      return(scores)
    }) %>%
    setNames(names(measures)) %>%
    dplyr::bind_cols(threshold = thresholds, .)
  if (!is.null(optimize)) {
    if (!optimize %in% names(measures))
      stop("optimize must be one of the measures being calculated. You provided ",
           "optimize = ", optimize, ", and measures = ", list_variables(names(measures)))
    best <- which.max(out_df[[optimize]] * measures[[optimize]])
    out_df$optimal <- FALSE
    out_df$optimal[best] <- TRUE
    attr(out_df, "optimized") <- optimize
  }

  class(out_df) <- c("thresholds_df", class(out_df))
  return(out_df)
}

get_measures <- function(measures) {
  available <- c("cost" = -1, "acc" = 1, "tpr" = 1, "fnr" = -1,
                 "tnr" = 1, "fpr" = -1, "ppv" = 1, "npv" = 1)
  if (length(measures) == 1 && measures == "all") {
    measures <- available
  } else {
    not_supported <- !measures %in% names(available)
    if (any(not_supported))
      stop("These are not available performance measures: ",
           list_variables(measures[not_supported]))
    measures <- available[measures]
  }
  return(measures)
}

#' Plot threshold performance metrics
#'
#' @param x A \code{threshold_df} object from \code{\link{get_thresholds}} or a
#'  data frame with columns "threshold" and other columns to be plotted against
#'  thresholds. If \code{optimize} was provided to \code{\link{get_thresholds}}
#'  a line is drawn in each facet corresponding to the optimal threshold.
#' @param title Plot title. Default NULL produces no title
#' @param caption Plot caption. Default NULL produces no caption unless
#' \code{get_thresholds(optimize)} was provided, in which case information
#' about the threshold and performance are provided in the caption.
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
#'
#' get_thresholds(m) %>%
#'   plot()
#'
#' get_thresholds(m, optimize = "cost", measures = c("acc", "cost"), cost_fn = 3) %>%
#'   plot(point_size = .5, ncol = 1)
plot.thresholds_df <- function(x, title = NULL, caption = NULL, font_size = 11,
                               line_size = .5, point_size = NA, ncol = 2,
                               print = TRUE, ... ) {
  if ( !is.data.frame(x) || !"threshold" %in% names(x) )
    stop("x must be a data frame from get_thresholds, or at least look like one!")

  # Process optimal
  optimized <- attr(x, "optimized")
  if (!is.null(optimized)) {
    thresh <- dplyr::filter(x, optimal) %>% dplyr::pull(threshold)
    x <- dplyr::select(x, -optimal)
    if (is.null(caption))
      caption <- paste("Threshold value of", signif(thresh, 3),
                       "chosen to optimize", optimized)
  }

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

  if (!is.null(optimized)) {
    the_plot <-
      the_plot +
      geom_vline(xintercept = thresh, linetype = "dashed",
                 alpha = .6, color = "firebrick")
  }

  if (print)
    print(the_plot)
  return(invisible(the_plot))
}
