context("Testing Feature Availability Profiler\n")


# ****************************************** Helpers ******************************************

sample_dataframe_with_dates = function(){
  df = data.frame(something=c(100,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))
  df['AdmitDTS']        = ('2017-03-20 00:00:00')
  df[1, 'AdmitDTS']     = ('2017-02-01 00:00:00')
  df[2, 'AdmitDTS']     = ('2017-02-02 00:00:00')
  df[3, 'AdmitDTS']     = ('2017-02-02 00:00:00')
  df[4, 'AdmitDTS']     = ('2017-02-03 00:00:00')
  df[5, 'AdmitDTS']     = ('2017-02-03 00:00:00')
  df[6, 'AdmitDTS']     = ('2017-02-04 00:00:00')
  df[7, 'AdmitDTS']     = ('2017-02-06 00:00:00')
  df[8, 'AdmitDTS']     = ('2017-02-10 00:00:00')
  df[9, 'AdmitDTS']     = ('2017-02-10 00:00:00')
  df[10, 'AdmitDTS']    = ('2017-02-10 00:00:00')

  # TODO this might be backwards bad data for the test
  df['LastLoadDTS']     = ('2017-01-19 00:00:00')
  df[1, 'LastLoadDTS']  = ('2017-02-19 00:00:00')

  return(df)
}

sample_dataframe_without_dates = function(){
  df = data.frame(something=c(100,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))
  return(df)
}

# ****************************************** percentNullsInDateRange ******************************************

test_that("percentNullsInDateRange works without a range", {
  # Sample dataframe
  df = sample_dataframe_without_dates()

  result = percentNullsInDateRange(df, 'something', 'thing')
  expect_that(result, is_a('numeric'))
  expect_equal(result, 20)
})

test_that("percentNullsInDateRange works with a range", {
  # Sample dataframe
  df = sample_dataframe_without_dates()

  result = percentNullsInDateRange(df, 'something', 'thing', 3, 5)
  expect_that(result, is_a('numeric'))
  expect_equal(result, 50)
})

test_that("percentNullsInDateRange works with a range that includes 0 records after filtering", {
  # Sample dataframe
  df = sample_dataframe_without_dates()

  result = percentNullsInDateRange(df, 'something', 'thing', 2, 2)
  expect_that(result, is_a('numeric'))
  expect_equal(result, 0)
})

test_that("percentNullsInDateRange works with a range that includes some records after filtering", {
  # Sample dataframe
  df = sample_dataframe_without_dates()

  result = percentNullsInDateRange(df, 'something', 'thing', 2, 3)
  expect_that(result, is_a('numeric'))
  expect_equal(result, 33.333333333)
})

test_that("percentNullsInDateRange throws errors on missing required arguments", {
  # Sample dataframe
  df = sample_dataframe_without_dates()

  expect_that(percentNullsInDateRange(), throws_error('Please specify a dataframe'))
  expect_that(percentNullsInDateRange(df), throws_error('Please specify an admit column name'))
  expect_that(percentNullsInDateRange(df, 'something'), throws_error('Please specify a feature column name'))
})


# ****************************************** hoursSinceAdmit ******************************************

test_that('hoursSinceAdmit works on strings', {
  hour0   = '2017-03-01 00:00:00'
  hour05  = '2017-03-01 00:30:00'
  hour1   = '2017-03-01 01:00:00'
  hour10  = '2017-03-01 10:00:00'
  hour24  = '2017-03-02 00:00:00'

  result1 = hoursSinceAdmit(hour0, hour1)
  expect_that(result1, is_a('numeric'))
  expect_equal(result1, 1)
  
  result05 = hoursSinceAdmit(hour0, hour05)
  expect_that(result05, is_a('numeric'))
  expect_equal(result05, 0.5)
  
  result10 = hoursSinceAdmit(hour0, hour10)
  expect_that(result10, is_a('numeric'))
  expect_equal(result10, 10)
  
  result24 = hoursSinceAdmit(hour0, hour24)
  expect_that(result24, is_a('numeric'))
  expect_equal(result24, 24)
})

test_that('hoursSinceAdmit works on lubradate parsed dates', {
  hour0   = ymd_hms('2017-03-01 00:00:00')
  hour05  = ymd_hms('2017-03-01 00:30:00')
  hour1   = ymd_hms('2017-03-01 01:00:00')
  hour10  = ymd_hms('2017-03-01 10:00:00')
  hour24  = ymd_hms('2017-03-02 00:00:00')

  result1 = hoursSinceAdmit(hour0, hour1)
  expect_that(result1, is_a('numeric'))
  expect_equal(result1, 1)
  
  result05 = hoursSinceAdmit(hour0, hour05)
  expect_that(result05, is_a('numeric'))
  expect_equal(result05, 0.5)
  
  result10 = hoursSinceAdmit(hour0, hour10)
  expect_that(result10, is_a('numeric'))
  expect_equal(result10, 10)
  
  result24 = hoursSinceAdmit(hour0, hour24)
  expect_that(result24, is_a('numeric'))
  expect_equal(result24, 24)
})

# ****************************************** featureAvailabilityProfiler ******************************************

test_that('featureAvailabilityProfiler throws errors on missing dataframe', {
  expect_that(featureAvailabilityProfiler(), throws_error('Please specify a dataframe'))
})

test_that('featureAvailabilityProfiler throws errors on non dataframe', {
  expect_that(featureAvailabilityProfiler('asdf'), throws_error('Please specify a dataframe'))
})

test_that('featureAvailabilityProfiler throws error on a dataframe with too few columns', {
  df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))
  
  expect_that(featureAvailabilityProfiler(df), throws_error('Dataframe must be at least 3 columns'))
})

test_that('featureAvailabilityProfiler throws error on a non-date columns', {
  df = sample_dataframe_with_dates()
  
  expect_that(featureAvailabilityProfiler(
    df, admitColumnName='AdmitDTS', lastLoadColumnName='thing'),
    throws_error('Last Load Date column is not a date type, or could not be parsed into one'))

  expect_that(featureAvailabilityProfiler(
    df, admitColumnName='something', lastLoadColumnName='AdmitDTS'),
    throws_error('Admit Date column is not a date type, or could not be parsed into one'))
})

# test_that('featureAvailabilityProfiler returns sensible results', {
#   df = sample_dataframe_with_dates()
   
#   result = featureAvailabilityProfiler(df, admitColumnName='AdmitDTS', lastLoadColumnName='LastLoadDTS', showPlot=FALSE, debug=FALSE)
  
#   expected_result = list(Age=list())

#   expect_that(result, is_a('list'))
#   expect_equal(result, expected_result)
# })
