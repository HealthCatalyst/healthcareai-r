#' Plot Counterfactual Predictions
#'
#' @param x A explore_df object from \code{\link{explore}}
#' @param n_use Number of features to vary, default = 4. If the number of
#'   features varied in \code{\link{explore}} is greater than \code{n_use},
#'   additional features will be aggregated over by \code{aggregate_fun}
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
#' @param sig_fig Number of significant figures (digits) to use in labels of
#'   numeric features. Default = 3; set to Inf to not truncate decimals.
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
#' @param title Plot title
#' @param caption Plot caption. Defaults to model used to make counterfactual
#'   predictions. Can be a string for custom caption or NULL for no caption.
#' @param print Print the plot? Default is FALSE. Either way, the plot is
#'   invisibly returned
#' @param ... Not used
#'
#' @return ggplot object, invisibly
#' @export
#'
#' @examples
#' # First, we need a model
#' set.seed(4956)
#' m <- machine_learn(pima_diabetes, patient_id, outcome = pregnancies,
#'                    models = "rf", tune = FALSE)
#' # Then we can explore our model through counterfactual predictions
#' counterfactuals <- explore(m)
#'
#' # By default only the two most important varying features are plotted. This
#' # example shows how counterfactual predictions can provide insight into how a
#' # model maps inputs (features) to the output (outcome). This plot shows that for
#' # this dataset, age is the most important predictor of the number of pregnancies
#' # a woman has had, and the predicted number of pregnancies rises basically
#' # linearly from approximately 20 to 40 and then levels off.
#' plot(counterfactuals)
#'
#' # To see the effects of more features in the model, increase the value of
#' # `n_use`. You can also specify which of the varying features are mapped to the
#' # x-axis and the color scale, and you can customize a variety of plot attributes
#' plot(counterfactuals, n_use = 3, x_var = weight_class, color_var = age,
#'     font_size = 9, strip_font_size = 1, line_width = 2, line_alpha = .5,
#'     rotate_x = TRUE, nrows = 1)
#'
#' # And you can further modify the plot like any other ggplot object
#' p <- plot(counterfactuals, n_use = 1, print = FALSE)
#' p +
#'   ylab("predicted number of pregnancies") +
#'   theme_classic() +
#'   theme(aspect.ratio = 1,
#'         panel.background = element_rect(fill = "slateblue"),
#'         plot.caption = element_text(face = "italic"))
plot.explore_df <- function(x, n_use = 2, aggregate_fun = median,
                            reorder_categories = TRUE, x_var, color_var,
                            jitter_y = TRUE, sig_fig = 3,
                            font_size = 11, strip_font_size = .85,
                            line_width = .5, line_alpha = .7,
                            rotate_x = FALSE, nrows = 1,
                            title = NULL, caption, print = TRUE, ...) {
  x_var <- rlang::enquo(x_var)
  color_var <- rlang::enquo(color_var)
  outcome <- stringr::str_subset(names(x), "^predicted_")
  alg <- attr(x, "model_info")$algorithm
  vi <- attr(x, "vi")

  if (n_use > 4)
    stop("counterfactual plots can only handle four varying features. ",
         "Set n_use to a value between 1 and 4.")
  if (nrow(vi) > n_use) {
    mes <- paste0("With ", nrow(vi), " varying features and n_use = ", n_use, ", using ",
                  substitute(aggregate_fun), " to aggregate predicted outcomes across ",
                  list_variables(vi$variable[-seq_len(n_use)]))
    if (n_use < 4)
      mes <- paste0(mes, ". You could turn `n_use` up to see the impact of more features.")
    message(mes)
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

  # Shorten appearance of numeric variables
  x <- dplyr::mutate_if(x, names(x) %in% vi$variable[vi$numeric], signif, sig_fig)


  # Determine variable-plot mappings
  vi <- map_variables(vi, x_var, color_var)

  p <-
    ggplot(x, aes(x = !!rlang::sym(vi$variable[vi$map_to == "x"]),
                  y = !!rlang::sym(outcome)))
  if ("color" %in% vi$map_to) {
    y_pos <-
      if (jitter_y) {
        position_jitter(width = 0, height = .01 * diff(range(x[[outcome]])))
      } else {
        "identity"
      }
    color_group <- rlang::sym(vi$variable[vi$map_to == "color"])
    p <- p +
      geom_line(aes(color = factor(!!color_group), group = !!color_group),
                position = y_pos, alpha = line_alpha, size = line_width) +
      scale_color_discrete(name = rlang::quo_name(color_group))
  } else {
    p <- p + geom_line(group = 1, alpha = line_alpha, size = line_width)
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

  if (missing(caption))
    caption <- paste("Predictions made by:", alg)
  p <- p +
    theme_gray(base_size = font_size) +
    theme(strip.text = element_text(size = rel(strip_font_size)),
          axis.text.x = x_text) +
    labs(title = title, caption = caption)

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
