#' Create a control chart
#' 
#' Create a control, aka Shewhart chart
#' \url{https://en.wikipedia.org/wiki/Control_chart}. 
#'
#' @param d data frame
#' @param measure variable of interest, to go on the y-axis
#' @param x variable to go on the x-axis, often a time variable. If unspecified
#' row indices will be used.
#' @param group1 Optional grouping variable to be panelled horizontally.
#' @param group2 Optional grouping variable to be panelled vertically.
#' @param center_line Function used to calculate central tendency. 
#' Defaults to mean.
#' @param sigmas Number of standard deviations above and below the central 
#' tendency to call a point influenced by "special cause variation".
#' Defaults to 3.
#' @param save_to Optional file path to save chart. If not provided, the chart
#' will be printed on screen.  
#' @param plot_width In inches. Only useful if save_to is specified.
#' @param plot_height In inches. Only useful if save_to is specified.
#' @param plot_title Title in upper-left.
#' @param plot_catpion Caption in lower-right.
#' @param plot_font_size Base font size; text elements will be scaled to this.
#'
#' @return Generally called for the side effect of printing the control chart
#' if save_to is not specified or saving the control chart to file if save_to
#' is specified. Invisibly, returns a ggplot object for further customization.
#' @export
#' @import ggplot2
#'
#' @examples
#' d <- tibble::data_frame(
#' day = sample(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 100, TRUE),
#' person = sample(c("Tom", "Jane", "Alex"), 100, TRUE),
#' count = rbinom(100, 20, ifelse(day == "Friday", .5, .2)),
#' date = Sys.Date() - sample.int(100))
#' control_chart(d, "count")
#' control_chart(d, "count", group1 = "day", group2 = "person")
#' if (require(ggplot2)) {
#' x <- control_chart(d, "count", "date")
#' x + 
#'   ylab("Number of Adverse Events") + 
#'   scale_x_date(name = "Week of", date_breaks = "week") +
#'   theme(axis.text.x = 
#'                    element_text(angle = -90, vjust = 0.5, hjust=1))
#' }
control_chart <- function(d, measure, x, group1, group2,
                          center_line = mean, sigmas = 3,
                          save_to, plot_width = 8, plot_height = 4,
                          plot_title = NULL, plot_catpion = NULL,
                          plot_font_size = 11) {

  theme_set(theme_bw())

  if (missing(d) || !(is.data.frame(d) || is.character(d))) {
    stop("You have to provide a data frame or a file location.")
  } else if (is.character(d)) {
    d <- readr::read_csv(d)
  }

  if (missing(measure)) {
    stop("You have to provide a measure variable name.")
  }

  if (missing(x)) {
    x <- "x"
    d$x <- seq_len(nrow(d))
  }

  bounds <- calculate_bounds(d, measure, center_line, sigmas)

  # Calculate central tendency and upper and lower limits
  d$outside <- ifelse(d[[measure]] > bounds[["upper"]] |
                        d[[measure]] < bounds[["lower"]], "out", "in")

  # Make plot
  chart <-
    ggplot(d, aes_string(x = x, y = measure)) +
    geom_hline(yintercept = bounds[["mid"]], color = "darkgray") +
    geom_hline(yintercept = c(bounds[["upper"]], bounds[["lower"]]),
                        linetype = "dotted", color = "darkgray") +
    geom_line() +
    geom_point(aes(color = outside), size = 2) +
    scale_color_manual(values = c("out" = "firebrick", "in" = "black"),
                                guide = FALSE) +
    labs(title = plot_title, caption = plot_catpion) +
    theme_bw(base_size = plot_font_size) +
    theme(panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank())

  # If grouping variables provided, facet by them
  if (!missing(group1) && !missing(group2)) {
    chart <-
      chart +
      facet_grid(as.formula(paste(group2, "~", group1)))
  } else if (!missing(group1)) {
    chart <-
      chart +
      facet_wrap(as.formula(paste("~", group1)), nrow = 1)
  } else if (!missing(group2)) {
    chart <-
      chart +
      facet_wrap(as.formula(paste("~", group2)), ncol = 1)
  }

  # If no file path, print plot, otherwise write to disk
  if (missing(save_to)) {
    print(chart)
  } else {
    ggsave(filename = save_to, plot = chart,
           width = plot_width, height = plot_height)
  }

  # Return the plot invisibly
  invisible(return(chart))
}

#' Calculate lower, middle, and upper lines for control_chart
#'
#' @param d 
#' @param measure 
#' @param center_line 
#' @param sigmas
#'
#' @return Named vector of three
#' @noRd
calculate_bounds <- function(d, measure, center_line, sigmas) {
  mid <- center_line(d[[measure]])
  sd3 <- sigmas * sd(d[[measure]])
  upper <- mid + sd3
  lower <- mid - sd3
  return(c(lower = lower, mid = mid, upper = upper))
}
