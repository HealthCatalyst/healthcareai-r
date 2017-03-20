context("Testing Feature Availability Profiler\n")

test_that("percentNullsInDateRange works without a range", {
  # Sample dataframe
  df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))

  result = percentNullsInDateRange(df, 'age', 'thing')
  expect_that(result, is_a('numeric'))
  expect_equal(result, 20)
})

test_that("percentNullsInDateRange works with a range", {
  # Sample dataframe
  df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))

  result = percentNullsInDateRange(df, 'age', 'thing', 3, 5)
  expect_that(result, is_a('numeric'))
  expect_equal(result, 50)
})

test_that("percentNullsInDateRange works with a range that includes 0 records after filtering", {
  # Sample dataframe
  df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))

  result = percentNullsInDateRange(df, 'age', 'thing', 2, 2)
  expect_that(result, is_a('numeric'))
  expect_equal(result, 0)
})

test_that("percentNullsInDateRange works with a range that includes some records after filtering", {
  # Sample dataframe
  df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))

  result = percentNullsInDateRange(df, 'age', 'thing', 2, 3)
  expect_that(result, is_a('numeric'))
  expect_equal(result, 33.333333333)
})

test_that("percentNullsInDateRange throws errors on missing required arguments", {
  # Sample dataframe
  df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))

  expect_that(percentNullsInDateRange(), throws_error('Please specify a dataframe'))
  expect_that(percentNullsInDateRange(df), throws_error('Please specify an admit column name'))
  expect_that(percentNullsInDateRange(df, 'age'), throws_error('Please specify a feature column name'))
})