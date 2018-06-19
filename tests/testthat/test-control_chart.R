context("Checking control_chart")

# Setup ------------------------------------------------------------------------
library(ggplot2)
test_df <- tibble::tibble(
  var1 = rep(letters[1:2], each = 10),
  var2 = rep(letters[2:5], times = 5),
  outcome = c(1:19, 25)  # To get different mean and median
)

# Test control_chart -----------------------------------------------------------
test_that("control_chart returns a ggplot object", {
  output <- control_chart(test_df, "outcome", print = FALSE)
  expect_true(is.ggplot(output))
  output <- control_chart(test_df, "outcome", group1 = "var1", group2 = "var2", print = FALSE)
  expect_true(is.ggplot(output))
})

test_that("control_chart takes csv filepath as argumnent", {
  write.csv(test_df, "tmpFile.csv", row.names = FALSE)
  output <- control_chart(d = "tmpFile.csv", measure = "outcome", print = FALSE)
  expect_true(is.ggplot(output))
})

test_that("control_chart errors if it doesn't get a data frame", {
  expect_error(control_chart())
  expect_error(control_chart(1))
  expect_error(control_chart(list(a = 1, b = 2)))
})

test_that("control_chart errors if grouping variables aren't present", {
  expect_error(control_chart(test_df, "outcome", group1 = "not_a_column"))
  expect_error(control_chart(test_df, "outcome", group1 = "var1", group2 = "n"))
  expect_error(control_chart(test_df, "outcome", group1 = "va"))
})

test_that("control_chart errors if measure or x column not present", {
  expect_error(control_chart(test_df, "outcome", group1 = "not_a_column"),
               regexp = "column")
  expect_error(control_chart(test_df, "outcome", group2 = "not_here"),
               regexp = "column")
  expect_error(control_chart(test_df, "outcome", x = "i'm not a variable"))
})

test_that("control_chart has correct number of panels", {
  item <- if (packageVersion("ggplot2") < "2.2.1.9000") "panel_layout" else "layout"
  output <- control_chart(test_df, "outcome", print = FALSE)
  expect_equal(nrow(ggplot_build(output)$layout[[item]]), 1L)
  output <- control_chart(test_df, "outcome", group1 = "var1", print = FALSE)
  expect_equal(nrow(ggplot_build(output)$layout[[item]]),
               length(unique(test_df$var1)))
  output <- control_chart(test_df, "outcome", group1 = "var1", group2 = "var2", print = FALSE)
  expect_equal(nrow(ggplot_build(output)$layout[[item]]),
               length(unique(test_df$var1)) * length(unique(test_df$var2)))
})

# Test calculate_bounds --------------------------------------------------------
bounds <- calculate_bounds(d = test_df,
                           measure = "outcome",
                           center_line = mean,
                           sigmas = 3)


test_that("calculate_bounds returns a vector of three with correct names", {
  expect_equal(length(bounds), 3L)
  expect_named(bounds)
  expect_identical(names(bounds), c("lower", "mid", "upper"))
})

test_that("calculate_bounds returns correct values with defaults", {
  expect_equal(bounds[["upper"]], 30, tolerance = .1)
  expect_equal(bounds[["mid"]], 10.75, tolerance = .1)
  expect_equal(bounds[["lower"]], -8.5, tolerance = .1)
})

test_that("calculate_bounds returns correct values with non-defaults", {

  new_bounds <- calculate_bounds(d = test_df,
                           measure = "outcome",
                           center_line = median,
                           sigmas = 2)

  expect_equal(new_bounds[["upper"]], 23.3, tolerance = .1)
  expect_equal(new_bounds[["mid"]], 10.5, tolerance = .1)
  expect_equal(new_bounds[["lower"]], -2.3, tolerance = .1)
})

# Clean up ---------------------------------------------------------------------
invisible(file.remove("tmpFile.csv"))
