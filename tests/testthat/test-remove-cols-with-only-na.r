context("Checking that columns are removed when every row in the column are NA's")

test_that("Data frame outputted is same as data frame inputted when every column 
          has no NA's", {
  df1 = data.frame(a = c(1,2,3),b = c('Y','N','Y'),c = c(FALSE,TRUE,FALSE),d = c(3,1,2))
  testthat::expect_identical(removeColsWithOnlyNA(df1),df1)
})

test_that("Data frame outputted is same as data frame inputted when every column 
          is not only NA's", {
  df1 = data.frame(a = c(1,NA,NA),b = c(NA,'N','Y'),c = c(FALSE,TRUE,NA),d = c(3,NA,2))
  testthat::expect_identical(removeColsWithOnlyNA(df1),df1)
})

test_that("Remove one column", {
  df1 = data.frame(a = c(1,1,NA),b = c('Y',NA,'Y'),c = c(FALSE,TRUE,FALSE),d = c(NA,NA,NA))
  resdf1 = data.frame(a = c(1,1,NA),b = c('Y',NA,'Y'),c = c(FALSE,TRUE,FALSE))
  testthat::expect_identical(removeColsWithOnlyNA(df1),resdf1)
  
  df2 = data.frame(a = c(1,2,1),b = c(NA,NA,NA),c = c(FALSE,TRUE,FALSE),d = c(NA,NA,2))
  resdf2 = data.frame(a = c(1,2,1),c = c(FALSE,TRUE,FALSE),d = c(NA,NA,2))
  testthat::expect_identical(removeColsWithOnlyNA(df2),resdf2)
})

test_that("Remove multiple columns", {
  df1 = data.frame(a = c(1,2,1),b = c('Y','Y','Y'),c = c(NA,NA,NA),d = c(NA,NA,NA))
  resdf = data.frame(a = c(1,2,1),b = c('Y','Y','Y'))
  testthat::expect_identical(removeColsWithOnlyNA(df1),resdf)
})

test_that("Remove all but one columns", {
  df1 = data.frame(a = c(1,2,1),b = c(NA,NA,NA),c = c(NA,NA,NA),d = c(NA,NA,NA))
  resdf = data.frame(a = c(1,2,1))
  testthat::expect_identical(removeColsWithOnlyNA(df1),resdf)
})

test_that("All columns are removed when all columns are NA's in every row", {
  df1 = data.frame(a = c(NA,NA,NA),b = c(NA,NA,NA),c = c(NA,NA,NA),d = c(NA,NA,NA))
  testthat::expect_output(removeColsWithOnlyNA(df1),
                "All columns were removed.")
})
