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
#'
#' @examples
#' d <- tibble::data_frame(
#' day = sample(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 100, TRUE),
#' person = sample(c("Tom", "Jane", "Alex"), 100, TRUE),
#' count = rbinom(100, 20, ifelse(day == "Friday", .5, .2)),
#' date = Sys.Date() - sample(100))
#' control_chart(d, "count")
#' control_chart(d, "count", group1 = "day", group2 = "person")
#' x <- control_chart(d, "count", "date")
#' x + 
#'   ggplot2::ylab("Number of Adverse Events") + 
#'   ggplot2::scale_x_date(name = "Week of", date_breaks = "week") +
#'   ggplot2::theme(axis.text.x = 
#'                    ggplot2::element_text(angle = -90, vjust = 0.5, hjust=1))
control_chart <- function(d, measure, x, group1, group2,
                          center_line = mean, sigmas = 3,
                          save_to, plot_width = 8, plot_height = 4,
                          plot_title = NULL, plot_catpion = NULL,
                          plot_font_size = 11) {

  ggplot2::theme_set(ggplot2::theme_bw())

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

  # Calculate central tendency and upper and lower limits
  mid <- center_line(d[[measure]])
  sd3 <- sigmas * sd(d[[measure]])
  upper <- mid + sd3
  lower <- mid - sd3
  d$outside <- ifelse(d[[measure]] > upper | d[[measure]] < lower, "out", "in")

  # Make plot
  chart <-
    ggplot2::ggplot(d, ggplot2::aes_string(x = x, y = measure)) +
    ggplot2::geom_hline(yintercept = mid, color = "darkgray") +
    ggplot2::geom_hline(yintercept = c(upper, lower),
                        linetype = "dotted", color = "darkgray") +
    ggplot2::geom_line() +
    ggplot2::geom_point(aes(color = outside), size = 2) +
    ggplot2::scale_color_manual(values = c("out" = "firebrick", "in" = "black"),
                                guide = FALSE) +
    ggplot2::labs(title = plot_title, caption = plot_catpion) +
    ggplot2::theme_bw(base_size = plot_font_size) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank())

  # If grouping variables provided, facet by them
  if (!missing(group1) && !missing(group2)) {
    chart <-
      chart +
      ggplot2::facet_grid(as.formula(paste(group2, "~", group1)))
  } else if (!missing(group1)) {
    chart <-
      chart +
      ggplot2::facet_wrap(as.formula(paste("~", group1)), nrow = 1)
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
