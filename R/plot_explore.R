#' Plot Counterfactual Predictions
#'
#' @param x A explore_df object from \code{\link{predict_counterfactuals}}
#' @param n_use Number of features to vary, default = 4. If the number of
#'   features varied in \code{\link{predict_counterfactuals}} is greater than
#'   \code{n_use}, additional features will be aggregated over by
#'   \code{aggregate_fun}
#' @param aggregate_fun Default = median. Varying features in x are mapped to
#'   the x-axis, line color, and vertical- and horizontal facets. If more than
#'   four features vary, this function is used to aggreagate across the
#'   least-important varying features.
#' @param reorder_categories If TRUE (default) varying categorical features are
#'   arranged by their median predicted outcome. If FALSE, the incoming level
#'   orders are retained, which is alphabetical by default, but you can set your
#'   own level orders with \code{reorder}
#' @param x_var Feature to put on the x-axis (unquoted). If not provided, the
#'   most important feature is used, with numerics prioritized if one varies
#' @param color_var Feature to color lines (unquoted). If not provided, the most
#'   important feature excluding \code{x_var} is used.
#' @param jitter_y If TRUE (default) and a feature is mapped to color (i.e. if
#'   there is more than one varying feature), the vertical location of the lines
#'   will be jittered slightly (no more than 1% of the range of the outcome) to
#'   avoid overlap.
#' @param font_size Parent font size for the plot. Default = 11
#' @param strip_font_size Relative font size for facet strip title font. Default
#'   = 0.85
#' @param line_width Width of lines. Default = 0.5
#' @param line_alpha Opacity of lines. Default = 0.7
#' @param rotate_x If FALSE (default), x axis tick labels are positioned
#'   horizontally. If TRUE, they are rotated one quarter turn, which can be
#'   helpful when a categorical feature with long labels is mapped to x.
#' @param nrows Only used when the number of varying features is three. The
#'   number of rows into which the facets will be arranged. Default = 1. NULL
#'   lets the number be determined algorithmically
#' @param print Print the plot? Default is FALSE. Either way, the plot is
#'   invisibly returned
#' @param ... Not used
#'
#' @return ggplot object, invisibly
#' @export
#'
#' @examples
#' # First, we need a model on which to make counterfactual predictions
#' set.seed(2507)
#' m <- machine_learn(pima_diabetes, patient_id, outcome = pregnancies,
#'                    tune = FALSE, models = "rf")
#' cfs <- predict_counterfactuals(m)
#'
#' # By default up to four varying features are plotted. This example shows how
#' # counterfactual predictions can provide insight into how a model maps inputs
#' # (features) to outputs (outcome). In this plot, across all other variables,
#' # we see a rapid rise in predict number of pregnancies from age 20 to ~40
#' # followed by a sharp leveling off the predicted number of pregnancies after
#' # age 40.
#' plot(cfs)
#'
#' # You can reduce the complexity of the plot by limiting the number of features
#' # varied in the plot. This is accomplished by averaging over the additional
#' # features using the `aggregate_fun`
#' plot(cfs, n_use = 2, aggregate_fun = mean)
#'
#' # Alternatively, you could vary only two features in the generation of counter-
#' # factual predictions
#' predict_counterfactuals(m, vary = 2) %>%
#'   plot()
#'
#' # You can specify which of the varying features are mapped to the x-axis and
#' # the color scale
#' plot(cfs, x_var = age, color_var = weight_class, n_use = 3)
#'
#' # There are a variety of options available to customize the appearance of the plot
#' plot(cfs, x_var = weight_class, color = diastolic_bp, n_use = 3,
#'      font_size = 16, strip_font_size = 1, line_width = 2, line_alpha = .5,
#'      rotate_x = TRUE, nrows = NULL)
#'
#' # And you can further modify the plot like any other ggplot object
#' plot(cfs, n_use = 1) +
#'   theme_classic() +
#'   labs(title = "Counterfactual predictions across age",
#'        caption = paste("Based on a random forest trained on",
#'                        nrow(pima_diabetes), "Pima women"))
plot.explore_df <- function(x, n_use = 4, aggregate_fun = median,
                            reorder_categories = TRUE, x_var, color_var,
                            jitter_y = TRUE, font_size = 11, strip_font_size = .85,
                            line_width = .5, line_alpha = .7,
                            rotate_x = FALSE, nrows = 1, print = TRUE, ...) {
  x_var <- rlang::enquo(x_var)
  color_var <- rlang::enquo(color_var)
  outcome <- stringr::str_subset(names(x), "^predicted_")

  vi <- attr(x, "vi")

  if (n_use > 4)
    stop("counterfactual plots can only handle four varying features. ",
         "Set n_use to a value between 1 and 4.")
  if (nrow(vi) > n_use) {
    message("With ", nrow(vi), " varying features and n_use = ", n_use, ", using ",
            substitute(aggregate_fun), " to aggregate predicted outcomes across ",
            list_variables(vi$variable[-seq_len(n_use)]))
    # Arrange variable indices by those specified first, then others in row order
    # which is feature importance order
    try_to_keep <- which(vi$variable %in% c(rlang::quo_name(x_var), rlang::quo_name(color_var)))
    to_keep <- dplyr::union(try_to_keep, seq_len(nrow(vi)))[seq_len(n_use)]
    vi <- slice(vi, to_keep)
    x <-
      x %>%
      group_by(!!!rlang::syms(vi$variable)) %>%
      summarize(!!rlang::sym(outcome) := aggregate_fun(!!rlang::sym(outcome))) %>%
      ungroup()
  }

  # Reorder categorical variables by median outcome value. This
  # also turns NA into a level so that it gets reordered
  if (reorder_categories)
    x <- dplyr::mutate_if(x, names(x) %in% vi$variable[!vi$numeric], ~ {
      as.character(.x) %>%
        tidyr::replace_na("NA") %>%
        reorder(- x[[outcome]], median)
    })

  # Determine variable-plot mappings
  vi <- map_variables(vi, x_var, color_var)

  p <-
    ggplot(x, aes(x = !!rlang::sym(vi$variable[vi$map_to == "x"]),
                  y = !!rlang::sym(outcome))) +
    if ("color" %in% vi$map_to) {
      y_pos <-
        if (jitter_y) {
          position_jitter(width = 0, height = .01 * diff(range(x[[outcome]])))
        } else {
          "identity"
        }
      color_group <- rlang::sym(vi$variable[vi$map_to == "color"])
      geom_line(aes(color = !!color_group, group = !!color_group),
                position = y_pos, alpha = line_alpha, size = line_width)
    } else {
      geom_line(group = 1, alpha = line_alpha, size = line_width)
    }

  facet_vars <- vi$variable[vi$map_to == "facet"]
  f <- function(x) formatC(x, drop0trailing = TRUE)
  if (length(facet_vars) == 1) {
    p <- p +
      facet_wrap(as.formula(paste("~", facet_vars)), nrow = nrows,
                 labeller = label_both)
  } else if (length(facet_vars) == 2) {
    p <-
      p +
      facet_grid(as.formula(paste(facet_vars[1], "~", facet_vars[2])),
                 labeller = label_both)
  }

  x_text <-
    if (rotate_x) {
      element_text(angle = 90, hjust = 1, vjust = 0.5)
    } else {
      element_text()
    }
  p <- p +
    theme_gray(base_size = font_size) +
    theme(strip.text = element_text(size = rel(strip_font_size)),
          axis.text.x = x_text)

  if (print)
    print(p)
  return(invisible(p))
}

# Choose how to map variables to plot aesthetics
map_variables <- function(vi, x_var, color_var) {

  if (!rlang::quo_is_missing(color_var) && nrow(vi) < 2)
    stop("You can't specify the color variable unless more than one feature varies")
  vi$map_to <- NA_character_

  # x axis
  if (rlang::quo_is_missing(x_var)) {
    # Order by numerics first
    ovars <- vi$variable[order(!vi$numeric)]
    # If x wasn't provided but color was, need to avoid the mapping color var to x
    if (!rlang::quo_is_missing(color_var))
      ovars <- ovars[ovars != rlang::quo_name(color_var)]
    vi$map_to[vi$variable == ovars[1]] <- "x"
  } else {
    x_var <- rlang::quo_name(x_var)
    if (!x_var %in% vi$variable)
      stop("You passed ", x_var, " to x_var, but it isn't a varying feature. ",
           "Either leave it blank or choose one of ", list_variables(vi$variable))
    vi$map_to[vi$variable == x_var] <- "x"
  }

  # color
  if (rlang::quo_is_missing(color_var)) {
    vi$map_to[which(is.na(vi$map_to))[1]] <- "color"
  } else {
    color_var <- rlang::quo_name(color_var)
    if (!color_var %in% vi$variable)
      stop("You passed ", color_var, " to color_var, but it isn't a varying feature. ",
           "Either leave it blank or choose one of ", list_variables(vi$variable))
    vi$map_to[vi$variable == color_var] <- "color"
  }

  # Others get faceted
  vi$map_to[is.na(vi$map_to)] <- "facet"

  return(vi)
}
