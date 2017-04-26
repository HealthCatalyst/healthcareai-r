context("Checking that the orderByDate function is working correctly")

# data frame is used to test multiple columns and dates without timestamps
df1 <- data.frame(a = c('2009-01-01','2010-01-01','2009-03-08','2009-01-19'),
                  b = c(1,2,3,4))

test_that("We have multiple columns and dates without timestamps and checking if ascending", {
  df2 <- data.frame(a = c('2009-01-01','2009-01-19','2009-03-08','2010-01-01'),
                    b = c(1,4,3,2))  
  rownames(df2) <- c(1,4,3,2)
  df2$a <- as.POSIXct(df2$a, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'a', descending = FALSE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})

test_that("We have multiple columns and dates without timestamps and checking if descending", {
  df2 <- data.frame(a = c('2010-01-01','2009-03-08','2009-01-19','2009-01-01'),
                    b = c(2,3,4,1))  
  rownames(df2) <- c(2,3,4,1)
  df2$a <- as.POSIXct(df2$a, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'a', descending = TRUE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})

# changing the data frame to test multiple columns and dates with timestamps
df1 <- data.frame(a = c('2009-03-08 12:59','2009-01-01 23:59','2010-01-01 23:59',
                        '2009-03-08 23:59','2009-01-19 23:59','2010-01-01 12:59'),
                  b = c(1,2,3,4,5,6))

test_that("We have multiple columns and dates with timestamps and checking if ascending", {
  df2 <- data.frame(a = c('2009-01-01 23:59','2009-01-19 23:59','2009-03-08 12:59',
                          '2009-03-08 23:59','2010-01-01 12:59','2010-01-01 23:59'),
                    b = c(2,5,1,4,6,3))  
  rownames(df2) <- c(2,5,1,4,6,3)
  df2$a <- as.POSIXct(df2$a, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'a', descending = FALSE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})

test_that("We have multiple columns and dates with timestamps and checking if descending", {
  df2 <- data.frame(a = c('2010-01-01 23:59','2010-01-01 12:59','2009-03-08 23:59',
                          '2009-03-08 12:59','2009-01-19 23:59','2009-01-01 23:59'),
                    b = c(3,6,4,1,5,2))  
  rownames(df2) <- c(3,6,4,1,5,2)
  df2$a <- as.POSIXct(df2$a, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'a', descending = TRUE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})

# changing the data frame to test one date column without timestamps
df1 <- data.frame(a = c('2009-01-01','2010-01-01','2009-03-08','2009-01-19'))

test_that("We have multiple date column without timestamps and checking if ascending", {
  df2 <- data.frame(a = c('2009-01-01','2009-01-19','2009-03-08','2010-01-01'))  
  colnames(df2) <- c("a")
  df2$a <- as.POSIXct(df2$a, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'a', descending = FALSE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})

test_that("We have one date column without timestamps and checking if descending", {
  df2 <- data.frame(a = c('2010-01-01','2009-03-08','2009-01-19','2009-01-01'))  
  colnames(df2) <- c("a")
  df2$a <- as.POSIXct(df2$a, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1,'a', descending = TRUE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})

# changing the data frame to test one date column with timestamps
df1 <- data.frame(a = c('2009-03-08 12:59','2009-01-01 23:59','2010-01-01 23:59',
                        '2009-03-08 23:59','2009-01-19 23:59','2010-01-01 12:59'))

test_that("We have one date column with timestamps and checking if ascending", {
  df2 <- data.frame(a = c('2009-01-01 23:59','2009-01-19 23:59','2009-03-08 12:59',
                          '2009-03-08 23:59','2010-01-01 12:59','2010-01-01 23:59'))  
  colnames(df2) <- c("a")
  df2$a <- as.POSIXct(df2$a, truncated = 5) # makes sure that df2 date is in POSIXct format
  df3 <- orderByDate(df1, 'a', descending = FALSE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than data frame object
})

test_that("We have one date column with timestamps and checking if descending", {
  df2 <- data.frame(a = c('2010-01-01 23:59','2010-01-01 12:59','2009-03-08 23:59',
                          '2009-03-08 12:59','2009-01-19 23:59','2009-01-01 23:59'))  
  colnames(df2) <- c("a")
  df2$a <- as.POSIXct(df2$a, truncated = 5)
  df3 <- orderByDate(df1, 'a', descending = TRUE)
  expect_equal(all(df3 == df2),TRUE) # just tests the the values rather than df object
})