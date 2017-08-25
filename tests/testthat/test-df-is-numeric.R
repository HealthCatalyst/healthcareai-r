context("Checking that a data frame is numeric")

test_that("True is returned when all columns are numeric w/o NA", {
  df <- data.frame(a = c(1,2,3,6,2), b = c(5,1,9,3,2))
  testthat::expect_equal(isNumeric(df), TRUE)
})

test_that("True is returned when all columns are numeric with NA", {
  df <- data.frame(a = c(1,2,3,6,2), b = c(NA,1,NA,3,2))
  testthat::expect_equal(isNumeric(df), TRUE)
})

test_that("True is returned when all columns are numeric with NA", {
  df <- data.frame(a = c(1,NA,3,6,2), b = c(NA,1,NA,3,2))
  testthat::expect_equal(isNumeric(df), TRUE)
})

test_that("False is returned when one column contains non-numeric values w/o NA", {
  df <- data.frame(a = c(1,2,3,6,2), b = c("a",1,8,3,2))
  testthat::expect_equal(isNumeric(df), FALSE)
})

test_that("False is returned when one column contains non-numeric values with NA", {
  df <- data.frame(a = c(1,2,3,6,2), b = c("a",1,8,NA,2))
  testthat::expect_equal(isNumeric(df), FALSE)
})

test_that("False is returned when two columns contain non-numeric values with NA", {
  df <- data.frame(a = c(1,"?",3,6,"b"), b = c("a",1,8,NA,2))
  testthat::expect_equal(isNumeric(df), FALSE)
})