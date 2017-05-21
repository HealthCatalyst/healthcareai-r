context('Checking featureAvailabilityProfiler')

test_that("For intra-day dataframe and NO dates specified return correctly", {

df1 <- data.frame(a = c(2,1,3,5,4,NA,7,NA),
                  b = c(0.7,-2,NA,-4,-5,-6,NA,NA),
                  c = c(100,300,200,NA,NA,NA,NA,500),
                  d = c(407,500,506,504,NA,NA,NA,405),
                  AdmitDTS = c('2012-01-01 00:00:00','2012-01-01 00:00:00',
                               '2012-01-01 03:00:00','2012-01-01 03:00:00',
                               '2012-01-01 06:00:00','2012-01-01 06:00:00',
                               '2012-01-01 09:00:00','2012-01-01 09:00:00'),
                  LastLoadDTS = c('2012-01-01 06:00:00','2012-01-01 06:00:00',
                                  '2012-01-01 06:00:00','2012-01-01 06:00:00',
                                  '2012-01-01 06:00:00','2012-01-01 06:00:00',
                                  '2012-01-01 06:00:00','2012-01-01 06:00:00'))

capture.output(actualOut1 <- featureAvailabilityProfiler(df = df1,
                                                         plotProfiler = FALSE))

expected1 <- list()
expected1$hoursSinceAdmit <- c(0, 1, 2, 3, 4, 6, 8, 12, 18, 24)
expected1$a <- c(50, NaN, NaN, 100, NaN, 100, NaN, NaN, NaN, NaN)
expected1$b <- c(100, NaN, NaN, 50, NaN, 100, NaN, NaN, NaN, NaN)
expected1$c <- c(0, NaN, NaN, 50, NaN, 100, NaN, NaN, NaN, NaN)
expected1$d <- c(0, NaN, NaN, 100, NaN, 100, NaN, NaN, NaN, NaN)

expect_equal(actualOut1$hoursSinceAdmit, expected1$hoursSinceAdmit)
expect_equal(actualOut1$a, expected1$a)
expect_equal(actualOut1$b, expected1$b)
expect_equal(actualOut1$c, expected1$c)
expect_equal(actualOut1$d, expected1$d)
})

test_that("For multi-day dataframe and dates specified return correctly", {
  df2 <- data.frame(a = c(2,1,3,5,4,NA,7,NA),
                    b = c(0.7,-2,NA,-4,-5,-6,NA,NA),
                    c = c(100,300,200,NA,NA,NA,NA,500),
                    d = c(407,500,506,504,NA,NA,NA,405),
                    admit = c('2012-01-01 00:00:00','2012-01-01 00:00:00',
                              '2012-01-01 12:00:00','2012-01-01 12:00:00',
                              '2012-01-02 00:00:00','2012-01-02 00:00:00',
                              '2012-01-02 12:00:00','2012-01-02 12:00:00'),
                    loaded = c('2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00'))
  
  capture.output(actualOut2 <- featureAvailabilityProfiler(df = df2,
                                            startDateColumn = 'admit',
                                            lastLoadDateColumn = 'loaded',
                                            plotProfiler = FALSE))
  
  expected2 <- list()
  expected2$hoursSinceAdmit <- c(0, 1, 2, 3, 4, 6, 8, 12, 18, 24, 48, 72)
  expected2$a <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, 50, NaN, 75, 100, NaN)
  expected2$b <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0, NaN, 75, 100, NaN)
  expected2$c <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, 50, NaN, 25, 100, NaN)
  expected2$d <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, 50, NaN, 50, 100, NaN)
  
  expect_equal(actualOut2$hoursSinceAdmit, expected2$hoursSinceAdmit)
  expect_equal(actualOut2$a, expected2$a)
  expect_equal(actualOut2$b, expected2$b)
  expect_equal(actualOut2$c, expected2$c)
  expect_equal(actualOut2$d, expected2$d)
})

test_that("Check that not in dataframe error arises", {
  df3 <- data.frame(a = c(2,1,3,5,4,NA,7,NA),
                    b = c(0.7,-2,NA,-4,-5,-6,NA,NA),
                    c = c(100,300,200,NA,NA,NA,NA,500),
                    d = c(407,500,506,504,NA,NA,NA,405),
                    admit = c('2012-01-01 00:00:00','2012-01-01 00:00:00',
                              '2012-01-01 12:00:00','2012-01-01 12:00:00',
                              '2012-01-02 00:00:00','2012-01-02 00:00:00',
                              '2012-01-02 12:00:00','2012-01-02 12:00:00'),
                    loaded = c('2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00'))
  
  
  expect_error(capture.output(featureAvailabilityProfiler(df = df3,
                                           startDateColumn = 'admit',
                                           lastLoadDateColumn = 'loade',
                                           plotProfiler = FALSE))
               ,paste0('loade is not in your dataframe. Please carefully',
                       ' specify the lastLoadDateColumn'))
  
})

test_that("Specific POSIX conversion error arises for startDateColumn", {
  df4 <- data.frame(a = c(2,1,3,5,4,NA,7,NA),
                    b = c(0.7,-2,NA,-4,-5,-6,NA,NA),
                    c = c(100,300,200,NA,NA,NA,NA,500),
                    d = c(407,500,506,504,NA,NA,NA,405),
                    admit = c('2012-01-01 00:00:00','2012-01-01 00:00:00',
                              '2012-01-01 12:00:00','2012-01-01 12:00:00',
                              '2012-01-02 00:00:00','2012-01-02 00:00:00',
                              '2012-01-02 12:00:00','2012-01-02 12:00:00'),
                    loaded = c('2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00'))
  
  
  expect_error(grepl(featureAvailabilityProfiler(df = df4,
                                                 startDateColumn = 'd',
                                                 lastLoadDateColumn = 'loaded',
                                                 plotProfiler = FALSE),     
               "d may not be a datetime column"))
})

test_that("Specific POSIX conversion error arises for lastLoadDateColumn", {
  df5 <- data.frame(a = c(2,1,3,5,4,NA,7,NA),
                    b = c(0.7,-2,NA,-4,-5,-6,NA,NA),
                    c = c(100,300,200,NA,NA,NA,NA,500),
                    d = c(407,500,506,504,NA,NA,NA,405),
                    admit = c('2012-01-01 00:00:00','2012-01-01 00:00:00',
                              '2012-01-01 12:00:00','2012-01-01 12:00:00',
                              '2012-01-02 00:00:00','2012-01-02 00:00:00',
                              '2012-01-02 12:00:00','2012-01-02 12:00:00'),
                    loaded = c('2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00',
                               '2012-01-03 00:00:00','2012-01-03 00:00:00'))
  
  
  expect_error(grepl(featureAvailabilityProfiler(df = df5,
                                                 startDateColumn = 'admit',
                                                 lastLoadDateColumn = 'b',
                                                 plotProfiler = FALSE),     
               "b may not be a datetime column"))
})