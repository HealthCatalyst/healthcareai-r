context("Checking that columns and dataframes are imputed correctly")

test_that("entire dataframe is imputed correctly", {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
    c=c(11,21,31,43), d=as.factor(c('Y','N','N',NA)))
  out <- imputeDF(df)
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=c(1,2,3,2), b=c('Y','N','Y','Y'),
    c=c(11,21,31,43), d=as.factor(c('Y','N','N','N')))
  valsExpected <- list(a=2, b='Y', c=26.5, d='N')
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
})


test_that('impute a subset of a data frame', {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
    c=c(11,21,31,43), d=c('Y','N','N',NA))
  df <- df[,c(1,2,4)]
  out <- imputeDF(df)
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=c(1,2,3,2), b=c('Y','N','Y','Y'), d=as.factor(c('Y','N','N','N')))
  valsExpected <- list(a=2, b='Y', d='N')
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
})


test_that('impute a whole data frame of factors in one step', {
  df <- data.frame(a=as.factor(c('apple','banana','kiwi','kiwi',NA)), 
                  b=as.factor(c('toaster','trumpet','roomba',NA, 'toaster')))
  out <- imputeDF(df)
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=as.factor(c('apple','banana','kiwi','kiwi','kiwi')), 
                b=as.factor(c('toaster','trumpet','roomba','toaster', 'toaster')))
  valsExpected <- list(a='kiwi', b='toaster')
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
})


test_that('impute a whole data frame of factors with 2 modes', {
  df <- data.frame(a=as.factor(c('apple','apple','kiwi','kiwi',NA)), 
                  b=as.factor(c('toaster','trumpet','trumpet',NA, 'toaster')))
  out <- imputeDF(df)
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=as.factor(c('apple','apple','kiwi','kiwi','apple')), 
                b=as.factor(c('toaster','trumpet','trumpet','toaster', 'toaster')))
  valsExpected <- list(a='apple', b='toaster')
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
})


test_that('impute a single column', {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
    c=c(11,21,31,43), d=c('Y','N','N',NA))
  df <- df['a']
  out <- imputeDF(df)
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=c(1,2,3,2))
  valsExpected <- list(a=2)
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
})


############### 2 STEPS ###############
test_that('impute a whole data frame in two steps', {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
    c=c(11,21,31,43), d=c('Y','N','N',NA))
  # Find values
  out <- imputeDF(df)
  imputeVals <- out$imputeVals
  # Insert values
  capture.output(out <- imputeDF(df,imputeVals))
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=c(1,2,3,2), b=c('Y','N','Y','Y'),
    c=c(11,21,31,43), d=as.factor(c('Y','N','N','N')))
  valsExpected <- list(a=2, b='Y', c=26.5, d='N')
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
})


test_that('impute a subset of a data frame in two steps', {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
    c=c(11,21,31,43), d=c('Y','N','N',NA))
  df <- df[,c(1,2,4)]
  # Find values
  out <- imputeDF(df)
  imputeVals <- out$imputeVals
  # Insert values
  capture.output(out <- imputeDF(df,imputeVals))
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=c(1,2,3,2), b=c('Y','N','Y','Y'), d=as.factor(c('Y','N','N','N')))
  valsExpected <- list(a=2, b='Y', d='N')
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
}) 


test_that('impute a whole data frame of factors in two steps', {
  df <- data.frame(a=as.factor(c('apple','banana','kiwi','kiwi',NA)), 
                  b=as.factor(c('toaster','trumpet','roomba',NA, 'toaster')))
  out <- imputeDF(df)
  imputeVals <- out$imputeVals
  # Insert values
  capture.output(out <- imputeDF(df,imputeVals))
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=as.factor(c('apple','banana','kiwi','kiwi','kiwi')), 
                b=as.factor(c('toaster','trumpet','roomba','toaster', 'toaster')))
  valsExpected <- list(a='kiwi', b='toaster')
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
})


test_that('impute a single column in two steps', {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
    c=c(11,21,31,43), d=c('Y','N','N',NA))
  df <- df['a']
  # Insert values
  out <- imputeDF(df)
  imputeVals <- out$imputeVals
  # Insert values
  capture.output(out <- imputeDF(df,imputeVals))
  dfOut <- out$df
  imputeVals <- out$imputeVals
  dfExpected <- data.frame(a=c(1,2,3,2))
  valsExpected <- list(a=2)
  expect_identical(dfOut, dfExpected)
  expect_identical(imputeVals, valsExpected)
})


test_that('collect values on a 4 column DF, apply them to 2 columns in two steps', {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
    c=c(11,21,31,43), d=as.factor(c('Y','N','N',NA)))
  # Find values for 4 columns
  out <- imputeDF(df)
  imputeVals <- out$imputeVals
  # Insert values for columns 2 and 4 only
  df <- df[,c(2,4)]
  colsToImpute <- c('b','d')
  capture.output(out <- imputeDF(df,imputeVals[colsToImpute]))
  dfOut <- out$df
  dfExpected <- data.frame(b=c('Y','N','Y','Y'),
    d=as.factor(c('Y','N','N','N')))
  valsExpected <- list(b='Y', d='N')
  expect_identical(dfOut, dfExpected)
})


############### ERROR HANDLING ###############
# collect values on a 4 column DF, apply them to 2 columns
test_that('throw an error when fed 4 column DF and only 2 values.', {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
    c=c(11,21,31,43), d=as.factor(c('Y','N','N',NA)))
  # Find values for 4 columns
  out <- imputeDF(df)
  imputeVals <- out$imputeVals
  # Insert values for columns 2 and 4 only
  colsToImpute <- c('b','d')
  expect_error(capture.output(imputeDF(df,imputeVals[colsToImpute])),
   'Your dataframe must have the same number of columns as your provided list!')
})


test_that('error when a column is all NA', {
  df <- data.frame(a=c(NA,NA,NA,NA), b=c('Y','N','Y','Y') )
  expect_error(imputeDF(df), 'replacement has length zero')
})


test_that('does nothing for a full column of numbers', {
  df <- data.frame(a=c(1,2,3,4,5))
  out <- imputeDF(df)
  dfOut <- out$df
  valsExpected <- list(a=3)
  imputeVals <- out$imputeVals
  expect_identical(dfOut, df)
  expect_identical(imputeVals, valsExpected)
})


test_that('does nothing for a full column of chars', {
  df <- data.frame(a=c('lion', 'tiger', 'liger', 'warthog','lion'), 
                  b=c('cheeseburger','bacon','pie','lentils', 'pie'))
  out <- imputeDF(df)
  dfOut <- out$df
  imputeVals <- out$imputeVals
  valsExpected <- list(a='lion', b='pie')
  expect_identical(dfOut, df)
  expect_identical(imputeVals, valsExpected)
})


test_that('does nothing for a full column of factors', {
  df <- data.frame(a=as.factor(c('lion', 'tiger', 'liger', 'warthog','lion')), 
                  b=as.factor(c('cheeseburger','bacon','pie','lentils', 'pie')))
  out <- imputeDF(df)
  dfOut <- out$df
  imputeVals <- out$imputeVals
  valsExpected <- list(a='lion', b='pie')
  expect_identical(dfOut, df)
  expect_identical(imputeVals, valsExpected)
})


test_that('error when df is not a dataframe.', {
  expect_error(imputeDF(c(1,2,3,4)), 'df must be a dataframe')
})


test_that('error when imputeVals is not a list.', {
   df <- data.frame(a=c(1,2,3,NA), b=c(11,21,31,43))
  expect_error(imputeDF(df, c(2,26.5)), 'imputeValues must be a list.')
})
