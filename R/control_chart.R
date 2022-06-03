#' Create a control chart
#'
#' Create a control chart, aka Shewhart chart:
#' \url{https://en.wikipedia.org/wiki/Control_chart}.
#'
#' @param d data frame or a path to a csv file that will be read in
#' @param measure variable of interest mapped to y-axis (quoted, ie as a string)
#' @param x variable to go on the x-axis, often a time variable. If unspecified
#'   row indices will be used (quoted)
#' @param group1 Optional grouping variable to be panelled horizontally (quoted)
#' @param group2 Optional grouping variable to be panelled vertically (quoted)
#' @param center_line Function used to calculate central tendency. Defaults to
#'   mean
#' @param sigmas Number of standard deviations above and below the central
#'   tendency to call a point influenced by "special cause variation." Defaults
#'   to 3
#' @param title Title in upper-left
#' @param catpion Caption in lower-right
#' @param font_size Base font size; text elements will be scaled to this
#' @param print Print the plot? Default = TRUE. Set to FALSE if you want to
#'   assign the plot to a variable for further modification, as in the last
#'   example.
#'
#' @return Generally called for the side effect of printing the control chart.
#'   Invisibly, returns a ggplot object for further customization.
#' @export
#' @import ggplot2
#' @importFrom tibble data_frame
#' @importFrom stats as.formula
#'
#' @examples
#' d <-
#'   tibble::tibble(
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
#'   scale_x_date(name = "Week of ... ", date_breaks = "week") +
#'   theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1))
control_chart <- function(d, measure, x, group1, group2, #nolint
                          center_line = mean, sigmas = 3,
                          title = NULL, catpion = NULL,
                          font_size = 11,
                          print = TRUE) {

  if (missing(d) || !(is.data.frame(d) || is.character(d))) {
    stop("You have to provide a data frame or a file location.")
  } else if (is.character(d)) {
    message("Attempting to read csv from ", d)
    d <- read.csv(d)
  }

  if (missing(measure)) {
    stop("You have to provide a measure variable name.")
  }

  if (!missing(group1) && !group1 %in% names(d))
    stop(group1, " isn't the name of a column in ", match.call()[["d"]])
  if (!missing(group2) && !group2 %in% names(d))
    stop(group2, " isn't the name of a column in ", match.call()[["d"]])

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
                                guide = "none") +
    labs(title = title, caption = catpion) +
    theme_gray(base_size = font_size) +
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

  if (print)
    print(chart)
  return(invisible(chart))
}

#' Calculate lower, middle, and upper lines for control_chart
#' @return Named vector of three
#' @noRd
calculate_bounds <- function(d, measure, center_line, sigmas) {
  mid <- center_line(d[[measure]])
  sd3 <- sigmas * stats::sd(d[[measure]])
  upper <- mid + sd3
  lower <- mid - sd3
  return(c(lower = lower, mid = mid, upper = upper))
}
