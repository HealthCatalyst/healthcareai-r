context("Checking functionality of nelsonRule1")

test_that("violations are correctly identified",{
  
  date <- seq.Date(from = as.Date('2016-01-03'), length.out = 52, by = 'week')
  set.seed(34)
  measureValue <- rnorm(length(date), mean = 100, sd = 15)

  # Alter some measureValues to be at least 3 standard deviations from the mean
  # so they can be displayed as violation examples.
  measureValue[9] <- 179
  measureValue[19] <- 22
  measureValue[47] <- 177

  d <- data.frame(date, measureValue)
  nr1 <- nelsonRule1(df = d, measure_col = 'measureValue', date_col = 'date')
  res <<- nr1$dfViolations[,c(1,5,6)]
  
  expected <<- data.frame(
    date = date[c(9,19,47)],
    violationFLG = c(1,1,1),
    violationDSC = c(
      'more than 3 standard deviations above the mean',
      'more than 3 standard deviations below the mean',
      'more than 3 standard deviations above the mean'
      )
    )
  
  # row.names(expected) <- c('9', '19', '47')
  row.names(res) <- NULL
  row.names(expected) <- NULL
  
  expect_equal(res, expected, tolerance = 1e-6)
})