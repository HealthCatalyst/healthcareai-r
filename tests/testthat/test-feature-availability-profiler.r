context("Testing Feature Availability Profiler\n")


# ****************************************** Helpers ******************************************

sample_dataframe_with_dates = function(){
  df = data.frame(something=c(100,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))
  df['AdmitDTS'] = as.Date('2017-03-20')
  df[1, 'AdmitDTS'] = as.Date('2017-02-01')
  df[2, 'AdmitDTS'] = as.Date('2017-02-02')
  df[3, 'AdmitDTS'] = as.Date('2017-02-02')
  df[4, 'AdmitDTS'] = as.Date('2017-02-03')
  df[5, 'AdmitDTS'] = as.Date('2017-02-03')
  df[6, 'AdmitDTS'] = as.Date('2017-02-04')
  df[7, 'AdmitDTS'] = as.Date('2017-02-06')
  df[8, 'AdmitDTS'] = as.Date('2017-02-10')
  df[9, 'AdmitDTS'] = as.Date('2017-02-10')
  df[10, 'AdmitDTS'] = as.Date('2017-02-10')
  df['LastLoadDTS'] = as.Date('2017-01-19')
  df[1, 'LastLoadDTS'] = as.Date('2017-02-19')

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


# ****************************************** featureAvailabilityProfiler ******************************************

test_that('featureAvailabilityProfiler throws errors on missing arguments', {
  df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))
  
  expect_that(featureAvailabilityProfiler(), throws_error('Please specify a dataframe'))
})

test_that('featureAvailabilityProfiler throws error on a dataframe with too few columns', {
  df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), other=c(NA,NA,NA,NA,NA,NA,1,2,3,4))
  
  expect_that(featureAvailabilityProfiler(df), throws_error('Dataframe must be at least 3 columns'))
})

test_that('featureAvailabilityProfiler throws error on a non-date columns', {
  df = sample_dataframe_with_dates()
  
  expect_that(featureAvailabilityProfiler(
    df, admitColumnName='something', lastLoadColumnName='AdmitDTS'),
    throws_error('Admit Date column is not a date type'))

  expect_that(featureAvailabilityProfiler(
    df, admitColumnName='AdmitDTS', lastLoadColumnName='thing'),
    throws_error('Last Load Date column is not a date type'))
})

test_that('featureAvailabilityProfiler returns sensible results', {
  df = sample_dataframe_with_dates()
   
  result = featureAvailabilityProfiler(df, admitColumnName='AdmitDTS', lastLoadColumnName='LastLoadDTS', showPlot=FALSE, debug=FALSE)
  
  expected_result = list(Age=list())

  expect_that(result, is_a('list'))
  expect_equal(result, expected_result)
})
