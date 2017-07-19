context("Checking that columns are removed when every row in the column has the same value")

test_that("Data frame outputted is same as data frame inputted when every column has rows with different values", {
  df1 = data.frame(a=c(1,2,3),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,1,2))
  expect_identical(removeColsWithAllSameValue(df1),df1)
})

test_that("Remove one column", {
  df1 = data.frame(a=c(1,1,1),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,1,NA))
  resdf = data.frame(b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE))
  expect_identical(removeColsWithAllSameValue(df1),resdf)

  df2 = data.frame(a=c(1,2,1),b=c('Y','Y','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,1,2))
  resdf = data.frame(a=c(1,2,1),c=c(FALSE,TRUE,FALSE),d=c(NA,1,2))
  expect_identical(removeColsWithAllSameValue(df2),resdf)

  df3 = data.frame(a=c(1,2,1),b=c('Y','N','Y'),c=c(FALSE,FALSE,FALSE),d=c(NA,1,2))
  resdf = data.frame(a=c(1,2,1),b=c('Y','N','Y'),d=c(NA,1,2))
  expect_identical(removeColsWithAllSameValue(df3),resdf)

  df4 = data.frame(a=c(1,2,1),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,NA,NA))
  resdf = data.frame(a=c(1,2,1),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE))
  expect_identical(removeColsWithAllSameValue(df4),resdf)
})

test_that("Remove multiple columns", {
  df1 = data.frame(a=c(1,2,1),b=c('Y','Y','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,NA,NA))
  resdf = data.frame(a=c(1,2,1),c=c(FALSE,TRUE,FALSE))
  expect_identical(removeColsWithAllSameValue(df1),resdf)
})

test_that("Remove all but one columns", {
  df1 = data.frame(a=c(1,2,1),b=c('Y','Y','Y'),c=c(FALSE,FALSE,FALSE),d=c(NA,NA,NA))
  resdf = data.frame(a=c(1,2,1))
  expect_identical(removeColsWithAllSameValue(df1),resdf)
})

test_that("All columns are removed when all columns have the same value in every row", {
  df1 = data.frame(a=c(1,1,1),b=c('Y','Y','Y'),c=c(FALSE,FALSE,FALSE),d=c(NA,NA,NA))
  expect_output(removeColsWithAllSameValue(df1),
                "All columns were removed.")
})
