context("Checking coefficient of variation (COV)")

test_that("vector WITHOUT nulls give correct result", {
  LOS = c(3.2,0.5,1.3,2.4,5)
  
  testthat::expect_equal(calculateCOV(LOS),
                         0.70)
})

test_that("vector col WITH nulls give correct result", {
  LOS = c(NA,0.5,1.3,2.4,6)
  
  testthat::expect_equal(calculateCOV(LOS),
                         0.95)
})

test_that("dataframe col WITHOUT nulls give correct result", {
  df <- data.frame(Gender = c('F','M','M','F','F'),
                   LOS = c(3.2,0.5,1.3,2.4,4))
  
  testthat::expect_equal(calculateCOV(df$LOS),
                         0.62)
})

test_that("dataframe col WITH nulls give correct result", {
  df <- data.frame(Gender = c('F','M','M','F','F'),
                   LOS = c(NA,0.5,1.3,2.4,4))
  
  testthat::expect_equal(calculateCOV(df$LOS),
                         0.74)
})

test_that("Vector of strings gives correct error", {
  Gender = c('F','M','M','F','F')
  
  testthat::expect_error(calculateCOV(Gender),
                         "Your vector must be of class numeric or integer")
})

test_that("dataframe col of strings gives correct error", {
  df <- data.frame(Gender = c('F','M','M','F','F'),
                   LOS = c(NA,0.5,1.3,2.4,4))
  
  testthat::expect_error(calculateCOV(df$Gender),
                         "Your vector must be of class numeric or integer")
})