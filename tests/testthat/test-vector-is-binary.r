context("Checking that a column is binary")

test_that("True is returned when just two values are present with NA", {
  expect_equal(isBinary(c(1,2,1,1,NA)), TRUE)
  expect_equal(isBinary(c(1,4,NA,1,1)), TRUE)
})

test_that("True is returned when just two values are present w/o NA", {
  expect_equal(isBinary(c(1,2,1,1,2)), TRUE)
  expect_equal(isBinary(c(1,4,4,1,1)), TRUE)
})

test_that("False is returned when more than two values are present with NA", {
  expect_equal(isBinary(c(1,2,5,NA,2)), FALSE)
  expect_equal(isBinary(c(1,4,4,2,NA)), FALSE)
})

test_that("False is returned when more than two values are present w/o NA", {
  expect_equal(isBinary(c(1,2,5,1,2)), FALSE)
  expect_equal(isBinary(c(1,4,4,2,2)), FALSE)
})
