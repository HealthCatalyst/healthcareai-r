context("Checking functionality of nelsonRule2")

test_that("violations are correctly identified",{
  
  date <- seq.Date(from = as.Date('2016-01-03'), length.out = 104, by = 'week')
  set.seed(34)
  measureValue <- rnorm(length(date), mean = 100, sd = 15)
  d <- data.frame(date, measureValue)
  
  # Alter some measureValues so a violation occurs
  d[['measureValue']][33:41] <- runif(n = 9,min = 105, max = 135)
  d[['measureValue']][78:87] <- runif(n = 10,min = 80, max = 95)
  
  nr <- nelsonRule2(d, 'measureValue', 'date')
  res <<- nr$dfViolations[,c(1,3,4)]
  
  expected <<- data.frame(
    date = date[c(40:41,83:87)],
    violationFLG = rep(1, 7),
    violationDSC = c(
      'At least 9 points in a row above the mean',
      'At least 9 points in a row above the mean',
      'At least 9 points in a row below the mean',
      'At least 9 points in a row below the mean',
      'At least 9 points in a row below the mean',
      'At least 9 points in a row below the mean',
      'At least 9 points in a row below the mean'
    )
  )
  
  row.names(res) <- NULL
  row.names(expected) <- NULL
  
  expect_equal(res, expected, tolerance = 1e-6)
})

