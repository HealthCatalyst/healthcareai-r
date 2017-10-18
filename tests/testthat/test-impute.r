# Tests:
# Test imputation using one function call for the following scenarios:
# 1. Entire dataframe, character and numeric inputs
# 2. Only a subset of a dataframe
# 3. Factor inputs
# 4. Factor inputs with 2 modes (uses alphabetical ranking)
# 5. Irrelevant list of impute values throws warning but doesn't get in the way
# 6. When a non-default value is provided it is used and doesn't interfere with defaults
# a. All columns are provided in unnamed list
# b. Named list of imputeVals in different order than df columns is provided
# c. Only the non-default column is provided
# 7. Impute values with names not in df are ignored with warning
# 8. NAs are honored as imputation values
# Test imputation using 2 function calls (first saves the values, second applies them):
# 1. Entire dataframe, character and numeric inputs
# 2. Only a subset of a dataframe
# 3. Factor inputs
# 4. Factor inputs with 2 modes (uses alphabetical ranking)
# 5. Calculate values for 4 columns, apply them correctly to 2 columns.
# 6. When an impute value would be a new level to be applied and another is irrelevant
# Error Handling:
# 1. Different length args
# 2. An all-NA column
# 3. No effect on full columns of factors, numbers, and characters
# 4. When inputs are not correct types
# 5. Unnamed list not of same length as df produces error
# 6. Partially named list produces error
# 7. Unnamed imputeVals with different variable types than df errors

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

test_that("Irrelevant list of impute values throws warning but doesn't get in the way", {
  original <- data.frame(x = c(3, -3, NA, 0), y = c("A", "A", "B", NA))
  default_target <- list(df = data.frame(x = c(3, -3, 0, 0),
                                         y = c("A", "A", "B", "A")),
                         imputeVals = list(x = 0, y = "A"))
  expect_warning(out <- imputeDF(original, list(a = 1, b = "three")))
  expect_identical(out, default_target)
})

test_that("When a non-default value is provided it is used and doesn't interfere with defaults", {
  original <- data.frame(x = c(3, -3, NA, 0), y = c("A", "A", "B", NA))
  custom_target <- list(df = data.frame(x = c(3, -3, 0, 0),
                                        y = c("A", "A", "B", "C")),
                        imputeVals = list(x = 0, y = "C"))
  expect_identical(imputeDF(original, list(0, "C")), custom_target)  
  expect_identical(imputeDF(original, list(y = "C", x = 0)), custom_target)  
  expect_identical(imputeDF(original, list(y = "C")), custom_target)  
})

test_that("Impute values with names not in df are ignored with warning", {
  original <- data.frame(x = c(3, -3, NA, 0), y = c("A", "A", "B", NA))
  custom_target <- list(df = data.frame(x = c(3, -3, 0, 0),
                                        y = c("A", "A", "B", "C")),
                        imputeVals = list(x = 0, y = "C"))
  expect_warning(expect_identical(
    imputeDF(original, list("z" = 5, y = "C")), custom_target
  ))
  expect_warning(expect_identical(
    imputeDF(original, list(y = "C", "z" = 5)), custom_target
  ))
})

test_that("NAs are honored as imputation values", {
  original <- data.frame(x = c(3, -3, NA, 0), y = c("A", "A", "B", NA))
  iv <- list(x = NA, y = "C")
  target <- list(df = data.frame(x = c(3, -3, NA, 0),
                                 y = c("A", "A", "B", "C")),
                 imputeVals = list(x = NA, y = "C"))
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


test_that('Apply values that are not present in the dataframe,
          discard unused impute values with a warning.', {
            df <- data.frame(a=as.factor(c('bagel','muffin','toast',NA)), 
                             b=as.factor(c('juice','water','milk',NA)))
            # Assing different values
            imputeVals <- list(b='pancake', d='wine')
            expect_warning(
              capture.output(out <- imputeDF(df,imputeVals))
            )
            dfOut <- out$df
            dfExpected <- data.frame(a=as.factor(c('bagel','muffin','toast','bagel')), 
                                     b=as.factor(c('juice','water','milk','pancake')))
            expect_identical(dfOut, dfExpected)
          })


############### ERROR HANDLING ###############
# collect values on a 4 column DF, apply them to 2 columns
# This should no longer throw an error. Rather, the two named values
# get used and the others have their values calculated in the second imputeDF call
test_that('No error when fed 4 column DF and 2 named values.', {
  df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
                   c=c(11,21,31,43), d=as.factor(c('Y','N','N',NA)))
  # Find values for 4 columns
  out <- imputeDF(df)
  imputeVals <- out$imputeVals
  # Insert values for columns 2 and 4 only
  colsToImpute <- c('b','d')
  expect_identical(imputeDF(df, imputeVals[colsToImpute]),
                   imputeDF(df))
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

test_that("Unnamed list not of same length as df produces error", {
  original <- data.frame(x = c(3, -3, NA, 0), y = c("A", "A", "B", NA))
  expect_error(imputeDF(original, list("C")))
  expect_error(imputeDF(original, list("C", 1, 3, 4)))
})

test_that("Partially named list produces error", {
  original <- data.frame(x = c(3, -3, NA, 0), y = c("A", "A", "B", NA))
  expect_error(imputeDF(original, list("C", name = 4)))
})

test_that("Unnamed imputeVals with different variable types than df errors", {
  original <- data.frame(x = c(3, -3, NA, 0), y = c("A", "A", "B", NA))
  expect_error(imputeDF(original, list("apple", "banana")))
  expect_error(imputeDF(original, list(3, 0)))
  expect_error(imputeDF(original, list("APPLE!!!", 0)))
})