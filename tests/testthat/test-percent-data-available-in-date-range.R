context('Checking percentDataAvailableInDateRange')

test_that("Calculation is correct for simple dataframe w/o date col", {
  df1 <- data.frame(a = c(1,2,NA,NA),
                    b = c('m','f','m','f'),
                    c = c(0.7,NA,2.4,-4),
                    d = c(100,300,200,NA),
                    e = c(400,500,NA,504))
  
  expected1 <- c(50, 100, 75, 75, 75)
  names(expected1) <- c('a','b', 'c', 'd', 'e')
  
  expect_equal(percentDataAvailableInDateRange(df1)[1],expected1[1])
  expect_equal(percentDataAvailableInDateRange(df1)[2],expected1[2])
  expect_equal(percentDataAvailableInDateRange(df1)[3],expected1[3])
  expect_equal(percentDataAvailableInDateRange(df1)[4],expected1[4])
  expect_equal(percentDataAvailableInDateRange(df1)[5],expected1[5])
})

test_that("Calculation is correct for simple dataframe w/ date col", {
  
  df2 <- data.frame(a = c(1,2,NA,NA),
                    b = c('m','f','m','f'),
                    c = c(0.7,NA,2.4,-4),
                    d = c(100,300,200,NA),
                    e = c(400,500,NA,504),
                    datecol = c('2012-01-01','2012-01-02',
                                '2012-01-03','2012-01-07'))
  
  actualOut <- percentDataAvailableInDateRange(df2,
                                               dateColumn = 'datecol',
                                               startInclusive = '2012-01-03',
                                               endExclusive = '2012-01-08')
  
  expected2 <- c(0, 100, 100, 50, 50)
  names(expected2) <- c('a','b', 'c', 'd', 'e')
  
  expect_equal(actualOut[1],expected2[1])
  expect_equal(actualOut[2],expected2[2])
  expect_equal(actualOut[3],expected2[3])
  expect_equal(actualOut[4],expected2[4])
  expect_equal(actualOut[5],expected2[5])
})

test_that("Error arises for missing dateColumn", {
  
  df3 <- data.frame(a = c(1,2,NA,NA),
                    b = c('m','f','m','f'),
                    datecol = c('2012-01-01','2012-01-02',
                                '2012-01-03','2012-01-07'))
  
  expect_error(percentDataAvailableInDateRange(df3,
                                               startInclusive = '2012-01-03',
                                               endExclusive = '2012-01-08'),
               "If any, specify dateColumn, startInclusive, AND endExclusive")
})

test_that("Specific error arises for missing startInclusive", {
  
  df4 <- data.frame(a = c(1,2,NA,NA),
                    b = c('m','f','m','f'),
                    datecol = c('2012-01-01','2012-01-02',
                                '2012-01-03','2012-01-07'))
  
  expect_error(percentDataAvailableInDateRange(df4,
                                               dateColumn = 'datecol',
                                               endExclusive = '2012-01-08'),
               "If any, specify dateColumn, startInclusive, AND endExclusive")
})

test_that("Specific error arises for missing endExclusive", {
  
  df5 <- data.frame(a = c(1,2,NA,NA),
                    b = c('m','f','m','f'),
                    datecol = c('2012-01-01','2012-01-02',
                                '2012-01-03','2012-01-07'))
  
  expect_error(percentDataAvailableInDateRange(df5,
                                               dateColumn = 'datecol',
                                               startInclusive = '2012-01-03'),
               "If any, specify dateColumn, startInclusive, AND endExclusive")
})

test_that("Specific error arises for bad startInclusive format", {
  
  df5 <- data.frame(a = c(1,2,NA,NA),
                    b = c('m','f','m','f'),
                    datecol = c('2012-01-01','2012-01-02',
                                '2012-01-03','2012-01-07'))
  
  expect_error(grepl(percentDataAvailableInDateRange(df5,
                                               dateColumn = 'datecol',
                                               startInclusive = '2012-01',
                                               endExclusive = '2012-01-08'),
               "columns need dates to be in YYYY-MM-DD format"))
})

test_that("Specific error arises for bad endExclusive date format", {
  
  df5 <- data.frame(a = c(1,2,NA,NA),
                    b = c('m','f','m','f'),
                    datecol = c('2012-01-01','2012-01-02',
                                '2012-01-03','2012-01-07'))
  
  expect_error(grepl(percentDataAvailableInDateRange(df5,
                                               dateColumn = 'datecol',
                                               startInclusive = '2012-01-03',
                                               endExclusive = '2012-01'),
               "columns need dates to be in YYYY-MM-DD format"))
})