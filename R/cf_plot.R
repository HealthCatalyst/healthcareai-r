#' Plot Simulated Counterfactual Predictions
#'
#' @param x A cf_df object from \code{link{simulate}}
#' @param x_var Variable to put on the x-axis (unquoted). If not provided, the
#'   most important variable is used, with numerics prioritized if one was
#'   varied
#' @param color_var Variable to color lines (unquoted). If not provided, the
#'   most important variable excluding \code{x_var}
#' @param reorder_categories If TRUE (default) categorical variables that were
#'   varied in \code{simulate} will be arranged in decreasing order of their
#'   median predicted outcome
#' @param aggregate_fun Default = median. Varying features in x are mapped to
#'   the x-axis, line color, and vertical- and horizontal facets. If
#'   more than four features vary, this function is used to aggreagate across
#'   the least-important varying features.
#' @param jitter_y If TRUE (default) and a variable is mapped to color (i.e. if
#'   there is more than one varying variable), the vertical location of the
#'   lines will be jittered slightly (no more than 1% of the range of the
#'   outcome variable) to avoid overlap.
#' @param font_size Parent font size for the whole plot. Default = 11
#' @param strip_font_size Relative font size for facet strip title font. Default
#'   = 0.85
#' @param line_width Width of lines. Default = 0.5
#' @param line_alpha Opacity of lines. Default = 0.7
#' @param rotate_x If FALSE (default), x axis tick labels are positioned
#'   horizontally. If TRUE, they are rotated one quarter turn, which can be
#'   helpful when a categorical variable with long labels is mapped to x.
#' @param print Print the plot? Default is FALSE. Either way, the plot is
#'   invisbly returned
#' @param ...
#'
#' @return ggplot object, invisibly
#' @export
#'
#' @description
#' @details
#'
#' @examples
plot.cf_df <- function(x, reorder_categories = TRUE, n_use = 4, aggregate_fun = median,
                       jitter_y = TRUE, x_var, color_var,
                       font_size = 11, strip_font_size = .85,
                       line_width = .5, line_alpha = .7,
                       rotate_x = FALSE, print = TRUE, ...) {
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
    vi <- slice(vi, seq_len(n_use))
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
      geom_line(aes(color = !!rlang::sym(vi$variable[vi$map_to == "color"])),
                position = y_pos, alpha = line_alpha, size = line_width)
    } else {
      geom_line(group = 1, alpha = line_alpha, size = line_width)
    }

  facet_vars <- vi$variable[vi$map_to == "facet"]
  f <- function(x) formatC(x, drop0trailing = TRUE)
  if (length(facet_vars) == 1) {
    p <- p +
      facet_wrap(as.formula(paste("~", facet_vars)),
                 labeller = label_both)
  } else if (length(facet_vars) == 2) {
    p <-
      p +
      facet_grid(as.formula(paste(facet_vars[1], "~", facet_vars[2])),
                 labeller = label_both)
  }

  x_text <- if(rotate_x) element_text(angle = 90, hjust = 1, vjust = 0.5) else element_text()
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
    stop("You specify the color variable unless more than one feature varies")
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
    vi$map_to[vi$variable == rlang::quo_name(x_var)] <- "x"
  }

  # color
  if (rlang::quo_is_missing(color_var)) {
    vi$map_to[which(is.na(vi$map_to))[1]] <- "color"
  } else {
    color_var <- rlang::quo_name(color_var)
    vi$map_to[vi$variable == color_var] <- "color"
  }

  # Others get faceted
  vi$map_to[is.na(vi$map_to)] <- "facet"

  return(vi)
}
