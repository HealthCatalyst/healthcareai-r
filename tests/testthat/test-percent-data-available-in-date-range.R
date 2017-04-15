context('Checking percentDataAvailableInDateRange')

df1 <- data.frame(a = c(1,2,NA,NA),
                 b = c('m','f','m','f'),
                 c = c(0.7,NA,2.4,-4),
                 d = c(100,300,200,NA),
                 e = c(400,500,NA,504))

expected1 <- c(50, 100, 75, 75, 75)
names(expected1) <- c('a','b', 'c', 'd', 'e')

test_that("Calculation is correct for simple dataframe w/o date col", {
  expect_equal(percentDataAvailableInDateRange(df1)[1],expected1[1])
  expect_equal(percentDataAvailableInDateRange(df1)[2],expected1[2])
  expect_equal(percentDataAvailableInDateRange(df1)[3],expected1[3])
  expect_equal(percentDataAvailableInDateRange(df1)[4],expected1[4])
  expect_equal(percentDataAvailableInDateRange(df1)[5],expected1[5])
})


df2 <- data.frame(a = c(1,2,NA,NA),
                  b = c('m','f','m','f'),
                  c = c(0.7,NA,2.4,-4),
                  d = c(100,300,200,NA),
                  e = c(400,500,NA,504),
                  datecol = c('2012-01-01','2012-01-02',
                              '2012-01-03','2012-01-07'))

expected2 <- c(0, 100, 100, 50, 50)
names(expected2) <- c('a','b', 'c', 'd', 'e')

test_that("Calculation is correct for simple dataframe w date col", {
  actualOut <- percentDataAvailableInDateRange(df2,
                                               dateColumn = 'datecol',
                                               startInclusive = '2012-01-03',
                                               endExclusive = '2012-01-08')
  
  expect_equal(actualOut[1],expected2[1])
  expect_equal(actualOut[2],expected2[2])
  expect_equal(actualOut[3],expected2[3])
  expect_equal(actualOut[4],expected2[4])
  expect_equal(actualOut[5],expected2[5])
})