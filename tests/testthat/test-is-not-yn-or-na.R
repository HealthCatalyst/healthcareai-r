context('Checking that isnotYNOrNA is working')

test_that("For simple dataframe, function returns expected boolean output, 
          TRUE", {
  dat <- data.frame(a = c(0,1,1,0,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0))
  
  actualOut <- isnotYNOrNA(dat[, 1])
  
  expected <- TRUE
  
  expect_equal(actualOut, expected)
  
})

test_that("For simple dataframe, function returns expected boolean output, 
          FALSE", {
  dat <- data.frame(a = c(3, 4, 5, 6, 7, 8, 9))
  
  actualOut <- isnotYNOrNA(dat[, 1])
  
  expected <- TRUE
  
  expect_equal(actualOut, expected)
  
})

test_that("For simple dataframe, function returns expected boolean output, 
          FALSE", {
  dat <- data.frame(a = c('y', 'n', 'Y', 'y', 'n', 'Y', 'Y', 'n', 'N', 'Y'))
  
  actualOut <- isnotYNOrNA(dat[, 1])
  
  expected <- TRUE
  
  expect_equal(actualOut, expected)
  
})

test_that("For simple dataframe, function returns expected boolean output, 
          FALSE", {
  dat <- data.frame(a = c('Y', 'N', 'Y', 'N', 'Y', 'Y', 'Y', 'N', 'N', 'Y', 
                          NA, NA))
  
  actualOut <- isnotYNOrNA(dat[, 1])
  
  expected <- FALSE
  
  expect_equal(actualOut, expected)
  
})


test_that("For simple dataframe, function returns expected boolean output, 
          FALSE", {
  dat <- data.frame(a = c('Y', 'N', 'Y', 'N', 'Y', 'Y', 'Y', 'N', 'N', 'Y'))
  
  actualOut <- isnotYNOrNA(dat[, 1])
  
  expected <- FALSE
  
  expect_equal(actualOut, expected)
  
})