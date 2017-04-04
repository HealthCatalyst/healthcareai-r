library(testthat)
library(healthcareai)
context("Checking that the orderByDate function is working correctly")


df1 <- data.frame(date = c('2009-01-01','2010-01-01','2009-03-08','2009-01-19'),
                  a = c(1,2,3,4))


test_that("We have multiple columns and dates without timestamps and checking if ascending", {
  df2 <- data.frame(date = c('2009-01-01','2009-01-19','2009-03-08','2010-01-01'),
             a = c(1,4,3,2))  
  rownames(df2) <- c(1,4,3,2)
  df2$date <- lubridate::ymd_hms(df2$date, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'date', descending = FALSE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})


test_that("We have multiple columns and dates without timestamps and checking if descending", {
  df2 <- data.frame(date = c('2010-01-01','2009-03-08','2009-01-19','2009-01-01'),
                    a = c(2,3,4,1))  
  rownames(df2) <- c(2,3,4,1)
  df2$date <- lubridate::ymd_hms(df2$date, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'date', descending = TRUE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})


df1 <- data.frame(date = c('2009-03-08 12:59','2009-01-01 23:59','2010-01-01 23:59','2009-03-08 23:59','2009-01-19 23:59','2010-01-01 12:59'),
                  a = c(1,2,3,4,5,6))

test_that("We have multiple columns and dates with timestamps and checking if ascending", {
  df2 <- data.frame(date = c('2009-01-01 23:59','2009-01-19 23:59','2009-03-08 12:59','2009-03-08 23:59','2010-01-01 12:59','2010-01-01 23:59'),
                    a = c(2,5,1,4,6,3))  
  rownames(df2) <- c(2,5,1,4,6,3)
  df2$date <- lubridate::ymd_hms(df2$date, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'date', descending = FALSE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})


test_that("We have multiple columns and dates with timestamps and checking if descending", {
  df2 <- data.frame(date = c('2010-01-01 23:59','2010-01-01 12:59','2009-03-08 23:59','2009-03-08 12:59','2009-01-19 23:59','2009-01-01 23:59'),
                    a = c(3,6,4,1,5,2))  
  rownames(df2) <- c(3,6,4,1,5,2)
  df2$date <- lubridate::ymd_hms(df2$date, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'date', descending = TRUE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})


df1 <- data.frame(date = c('2009-01-01','2010-01-01','2009-03-08','2009-01-19'))


test_that("We have multiple date column without timestamps and checking if ascending", {
  df2 <- data.frame(date = c('2009-01-01','2009-01-19','2009-03-08','2010-01-01'))  
  colnames(df2) <- c("date")
  df2$date <- lubridate::ymd_hms(df2$date, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'date', descending = FALSE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})


test_that("We have one date column without timestamps and checking if descending", {
  df2 <- data.frame(date = c('2010-01-01','2009-03-08','2009-01-19','2009-01-01'))  
  colnames(df2) <- c("date")
  df2$date <- lubridate::ymd_hms(df2$date, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'date', descending = TRUE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})


df1 <- data.frame(date = c('2009-03-08 12:59','2009-01-01 23:59','2010-01-01 23:59','2009-03-08 23:59','2009-01-19 23:59','2010-01-01 12:59'))

test_that("We have one date column with timestamps and checking if ascending", {
  df2 <- data.frame(date = c('2009-01-01 23:59','2009-01-19 23:59','2009-03-08 12:59','2009-03-08 23:59','2010-01-01 12:59','2010-01-01 23:59'))  
  colnames(df2) <- c("date")
  df2$date <- lubridate::ymd_hms(df2$date, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1, 'date', descending = FALSE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})

test_that("We have one date column with timestamps and checking if descending", {
  df2 <- data.frame(date = c('2010-01-01 23:59','2010-01-01 12:59','2009-03-08 23:59','2009-03-08 12:59','2009-01-19 23:59','2009-01-01 23:59'))  
  colnames(df2) <- c("date")
  df2$date <- lubridate::ymd_hms(df2$date, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1, 'date', descending = TRUE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})



