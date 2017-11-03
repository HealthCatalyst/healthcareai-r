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
  output <- control_chart(test_df, "outcome")
  expect_true(is.ggplot(output))
  output <- control_chart(test_df, "outcome", group1 = "var1", group2 = "var2")
  expect_true(is.ggplot(output))
  output <- control_chart(test_df, "outcome", group1 = "var1", group2 = "var2",
                          save_to = "tmpFile.png")
  expect_true(is.ggplot(output))
})

test_that("control_chart writes file if given a save_to path", {
  suppressWarnings(invisible(file.remove("tmpFile.png")))
  output <- control_chart(test_df, "outcome", save_to = "tmpFile.png")
  expect_true(file.exists("tmpFile.png"))
})

test_that("no file written when filename isn't provided", {
  n_files <- list.files()
  x <- control_chart(test_df, "outcome")
  expect_equal(n_files, list.files())
})

test_that("control_chart takes csv filepath as argumnent", {
  readr::write_csv(test_df, "tmpFile.csv")
  output <- control_chart(d = "tmpFile.csv", measure = "outcome")
  expect_true(is.ggplot(output))
  control_chart(d = "tmpFile.csv", measure = "outcome", save_to = "tmpFile.png")
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

test_that("control_chart errors if measure column present", {
  expect_error(control_chart(test_df, group1 = "not_a_column"))
})

test_that("control_chart errors if save_to doesn't look like an image file", {
  expect_error(control_chart(test_df, "outcome", save_to = "file"))
  expect_error(control_chart(test_df, "outcome", save_to = "file.notextension"))
  # test there isn't an error if extension is all caps
  expect_error(control_chart(test_df, "outcome", save_to = "tmpFile.PNG"),
               regexp = NA)
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
file.remove(c("tmpFile.png", "tmpFile.PNG", "tmpFile.csv"))
