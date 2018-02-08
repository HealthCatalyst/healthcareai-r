context("ID numeric columns in data frame")

#### Setup
test_d <- data.frame(x = 1:5, y = letters[1:5], z = rnorm(5))

#### Tests
test_that("find_numeric_columns finds numbers", {
  expect_equal(find_numeric_columns(test_d), c("x", "z"))
})
