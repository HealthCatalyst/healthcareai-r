context('Checking that isZeroOne is working')

test_that("For simple dataframe, function returns expected boolean output, TRUE", {
  dat <- data.frame(a = c(0,1,1,0,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0))
  
  actualOut <- isZeroOne(dat[, 1])
  
  expected <- TRUE
  
  expect_equal(actualOut, expected)
  
})

test_that("For simple dataframe, function returns expected error, FALSE", {
  dat <- data.frame(a = c(3, 4, 5, 6, 7, 8, 9))
  
  actualOut <- isZeroOne(dat[, 1])
  
  expected <- FALSE
  
  expect_equal(actualOut, expected)
  
})