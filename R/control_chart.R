#' Create a control chart
#' 
#' Create a control chart, aka Shewhart chart:
#' \url{https://en.wikipedia.org/wiki/Control_chart}. 
#'
#' @param d data frame or a path to a csv file that will be read in
#' @param measure variable of interest mapped to y-axis (quoted, ie as a string)
#' @param x variable to go on the x-axis, often a time variable. If unspecified
#' row indices will be used (quoted)
#' @param group1 Optional grouping variable to be panelled horizontally (quoted)
#' @param group2 Optional grouping variable to be panelled vertically (quoted)
#' @param center_line Function used to calculate central tendency. 
#' Defaults to mean
#' @param sigmas Number of standard deviations above and below the central 
#' tendency to call a point influenced by "special cause variation."
#' Defaults to 3
#' @param save_to Optional file path to save chart. If not provided, the chart
#' will be printed on screen
#' @param plot_width In inches. Only used if save_to is specified
#' @param plot_height In inches. Only used if save_to is specified
#' @param plot_title Title in upper-left
#' @param plot_catpion Caption in lower-right
#' @param plot_font_size Base font size; text elements will be scaled to this
#'
#' @return Generally called for the side effect of printing the control chart
#' or writing the control chart to file, depending on whether save_to is 
#' specified. Invisibly, returns a ggplot object for further customization.
#' @export
#' @import ggplot2
#' @importFrom readr read_csv
#' @importFrom tibble data_frame
#' @importFrom stats as.formula
#'
#' @examples
#' # Create a data frame to plot
#' 
#' d <- 
#'   tibble::data_frame(
#'     day = sample(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
#'                  100, TRUE),
#'     person = sample(c("Tom", "Jane", "Alex"), 100, TRUE),
#'     count = rbinom(100, 20, ifelse(day == "Friday", .5, .2)),
#'     date = Sys.Date() - sample.int(100))
#'
#' # Minimal arguments are the data and the column to put on the y-axis.
#' # If x is not provided, observations will be plotted in order of the rows
#' 
#' control_chart(d, "count")
#' 
#' # Specify categorical variables for group1 and/or group2 to get a separate
#' # panel for each category
#' 
#' control_chart(d, "count", group1 = "day", group2 = "person")
#' 
#' # In addition to printing or writing the plot to file, control_chart
#' # returns the plot as a ggplot2 obejct, which you can then further customize
#' 
#' library(ggplot2)
#' my_chart <- control_chart(d, "count", "date")
#' my_chart + 
#'   ylab("Number of Adverse Events") + 
#'   scale_x_date(name = "Week of", date_breaks = "week") +
#'   theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1))
#' 
control_chart <- function(d, measure, x, group1, group2,
                          center_line = mean, sigmas = 3,
                          save_to, plot_width = 8, plot_height = 4,
                          plot_title = NULL, plot_catpion = NULL,
                          plot_font_size = 11) {

  if (missing(d) || !(is.data.frame(d) || is.character(d))) {
    stop("You have to provide a data frame or a file location.")
  } else if (is.character(d)) {
    message("Attempting to read csv from ", d)
    d <- readr::read_csv(d)
  }

  if (missing(measure)) {
    stop("You have to provide a measure variable name.")
  }

  if (!missing(group1) && !group1 %in% names(d))
    stop(group1, " isn't the name of a column in ", match.call()[["d"]])
  if (!missing(group2) && !group2 %in% names(d))
    stop(group2, " isn't the name of a column in ", match.call()[["d"]])

  if (!missing(save_to) && !grepl("\\.[[:alpha:]]{3,4}$", save_to))
    stop("save_to has to end with a graphics extension such as .png or .pdf")

  if (missing(x)) {
    x <- "x"
    d$x <- seq_len(nrow(d))
  } else if (!x %in% names(d)) {
    stop("You provided x = \"", x,
         "\" but that isn't the name of a column in ", match.call()[["d"]])
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
      facet_grid(stats::as.formula(paste(group2, "~", group1)))
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
