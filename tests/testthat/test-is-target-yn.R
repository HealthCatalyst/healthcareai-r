context('Checking that isnotYNOrNA is working')

test_that("For a vector of all 0's and 1's, function returns expected output,
          TRUE", {
  
  dat <- data.frame(a = c(0,1,1,0,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0))
  
  actualOut <- isTargetYN(dat[, 1])
  
  expected <- FALSE
  
  expect_equal(actualOut, expected)
  
})

test_that("For a vector of all numbers, function returns expected output, 
          FALSE", {
  
  dat <- data.frame(a = c(3, 4, 5, 6, 7, 8, 9))
  
  actualOut <- isTargetYN(dat[, 1])
  
  expected <- FALSE
  
  expect_equal(actualOut, expected)
  
})

test_that("For a vector of Y's, y's, N's, and n's, function returns expected 
          output, FALSE", {
  
  dat <- data.frame(a = c('y', 'n', 'Y', 'y', 'n', 'Y', 'Y', 'n', 'N', 'Y'))
  
  actualOut <- isTargetYN(dat[, 1])
  
  expected <- FALSE
  
  expect_equal(actualOut, expected)
  
})

test_that("For a simple vector of Y's, N's, and NA's, function returns expected 
          output, TRUE", {
  
  dat <- data.frame(a = c('Y', 'N', NA, 'N', 'Y', 'Y', NA, 'N', 'N', 'Y', 
                          NA, NA))
  
  actualOut <- isTargetYN(dat[, 1])
  
  expected <- TRUE
  
  expect_equal(actualOut, expected)
  
})


test_that("For a simple vector of Y's and N's, function returns expected output, 
          TRUE", {
  
  dat <- data.frame(a = c('Y', 'N', 'Y', 'N', 'Y', 'Y', 'Y', 'N', 'N', 'Y'))
  
  actualOut <- isTargetYN(dat[, 1])
  
  expected <- TRUE
  
  expect_equal(actualOut, expected)
  
})