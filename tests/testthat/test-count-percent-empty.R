context('Checking that countPercentEmpty is working')

test_that("For simple dataframe, each column is calculated correctly", {
  df <- data.frame(a = c(NA,1,1), b = c(NA,NA,1), c = c(NA,NA,NA))
    
  actualOut <- suppressWarnings(countPercentEmpty(df))
  
  expected1 <- c(0.3333333, 0.6666667, 1.0000000)
  names(expected1) <- c('a','b','c')
  
  expect_equal(actualOut[1], expected1[1], tolerance = 1e-3)
  expect_equal(actualOut[2], expected1[2], tolerance = 1e-3)
  expect_equal(actualOut[3], expected1[3], tolerance = 1e-3)
})