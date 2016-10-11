context("Checking that rows are removed when they have NA in the specified columns")

test_that("Data frame outputted is same as data frame inputted when no NAs are present", {
  df1 = data.frame(a=c(1,2,3),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_identical(removeRowsWithNAInSpecCol(df1,'a'), df1)

  df2 = data.frame(a=c(1,2,3),b=c('Y','N','N'),c=c(TRUE,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_identical(removeRowsWithNAInSpecCol(df2,'b'), df2)
})

test_that("Data frame outputted is same as data frame inputted when no NAs are present in specified column but NAs are present", {
  df1 = data.frame(a=c(1,2,3),b=c(NA,'N','Y'),c=c(FALSE,TRUE,NA),stringsAsFactors = TRUE)
  expect_identical(removeRowsWithNAInSpecCol(df1,'a'), df1)

  df2 = data.frame(a=c(1,NA,3),b=c('Y','N','N'),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_identical(removeRowsWithNAInSpecCol(df2,'b'), df2)
})

test_that("Remove one row successfully", {
  df1=data.frame(a=c(NA,2,3,NA),b=c(NA,'N','Y',NA),c=c(NA,FALSE,NA,TRUE),stringsAsFactors = TRUE)
  df1original = data.frame(a=c(NA,2,3),b=c(NA,'N','Y'),c=c(TRUE,FALSE,NA),stringsAsFactors = TRUE)
  expect_equal(removeRowsWithNAInSpecCol(df1original,'a'), df1[2:3,])

  df2=data.frame(a=c(NA,2,3,NA),b=c(NA,'N','Y',NA),c=c(NA,FALSE,NA,TRUE),stringsAsFactors = FALSE)
  df2original = data.frame(a=c(NA,2,3),b=c(NA,'N','Y'),c=c(TRUE,FALSE,NA),stringsAsFactors = FALSE)
  expect_equal(removeRowsWithNAInSpecCol(df2original,'a'), df2[2:3,])

  df3=data.frame(a=c(1,NA,3,NA),b=c('N',NA,'Y',NA),c=c(NA,NA,TRUE,FALSE),stringsAsFactors = TRUE)
  df3original = data.frame(a=c(1,NA,3),b=c('N',NA,'Y'),c=c(NA,FALSE,TRUE),stringsAsFactors = TRUE)
  expect_identical(removeRowsWithNAInSpecCol(df3original,'b'), df3[c(1,3),])

  df4=data.frame(a=c(1,NA,3,NA),b=c('N',NA,'Y',NA),c=c(NA,NA,TRUE,FALSE),stringsAsFactors = FALSE)
  df4original = data.frame(a=c(1,NA,3),b=c('N',NA,'Y'),c=c(NA,FALSE,TRUE),stringsAsFactors = FALSE)
  expect_identical(removeRowsWithNAInSpecCol(df4original,'b'), df4[c(1,3),])
})

test_that("Remove one row wrong values", {
  df1=data.frame(a=c(NA,2,1,NA),b=c(NA,'N','Y',NA),c=c(NA,TRUE,NA,FALSE),stringsAsFactors = TRUE)
  df1original = data.frame(a=c(NA,2,3),b=c(NA,'N','Y'),c=c(FALSE,TRUE,NA),stringsAsFactors = TRUE)
  expect_false(identical(removeRowsWithNAInSpecCol(df1original,'a'), df1[2:3,]))

  df2=data.frame(a=c(1,NA,3,NA),b=c('N',NA,'N','Y'),c=c(NA,NA,FALSE,TRUE),stringsAsFactors = TRUE)
  df2original = data.frame(a=c(1,NA,3),b=c('Y',NA,'N'),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_false(identical(removeRowsWithNAInSpecCol(df2original,'b'), df2[c(1,3),]))

  df3=data.frame(a=c(NA,NA,3,NA),b=c('Y',NA,'N',NA),c=c(NA,NA,FALSE,TRUE),stringsAsFactors = TRUE)
  df3original = data.frame(a=c(1,NA,3),b=c('Y',NA,'N'),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_false(identical(removeRowsWithNAInSpecCol(df3original,'b'), df3[c(1,3),]))

  df4=data.frame(a=c(1,NA,3,NA),b=c('Y',NA,'N',NA),c=c(NA,TRUE,NA,FALSE),stringsAsFactors = TRUE)
  df4original = data.frame(a=c(1,NA,3),b=c('Y',NA,'N'),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_false(identical(removeRowsWithNAInSpecCol(df4original,'b'), df4[c(1,3),]))
})

test_that("Remove two rows successfully", {
  df1=data.frame(a=c(NA,NA,3,NA),b=c(NA,NA,'Y','N'),c=c(FALSE,NA,NA,TRUE),stringsAsFactors = TRUE)
  df1original = data.frame(a=c(NA,NA,3),b=c(NA,'N','Y'),c=c(FALSE,TRUE,NA),stringsAsFactors = TRUE)
  expect_identical(removeRowsWithNAInSpecCol(df1original,'a'), df1[3,])

  df2=data.frame(a=c(NA,NA,3,NA),b=c(NA,NA,'Y','N'),c=c(FALSE,NA,NA,TRUE),stringsAsFactors = FALSE)
  df2original = data.frame(a=c(NA,NA,3),b=c(NA,'N','Y'),c=c(FALSE,TRUE,NA),stringsAsFactors = FALSE)
  expect_identical(removeRowsWithNAInSpecCol(df2original,'a'), df2[3,])

  df3=data.frame(a=c(1,NA,NA,NA),b=c('Y',NA,NA,NA),c=c(NA,NA,FALSE,TRUE),stringsAsFactors = TRUE)
  df3original = data.frame(a=c(1,NA,3),b=c('Y',NA,NA),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_identical(removeRowsWithNAInSpecCol(df3original,'b'), df3[1,])

  df4=data.frame(a=c(1,NA,NA,NA),b=c('Y',NA,NA,NA),c=c(NA,NA,FALSE,TRUE),stringsAsFactors = FALSE)
  df4original = data.frame(a=c(1,NA,3),b=c('Y',NA,NA),c=c(NA,TRUE,FALSE),stringsAsFactors = FALSE)
  expect_identical(removeRowsWithNAInSpecCol(df4original,'b'), df4[1,])

  df5=data.frame(a=c(NA,2,NA,NA),b=c('Y',NA,NA,NA),c=c(NA,NA,FALSE,TRUE),stringsAsFactors = TRUE)
  df5original = data.frame(a=c(NA,2,NA),b=c('Y',NA,NA),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_identical(removeRowsWithNAInSpecCol(df5original,'b'), df5[1,])

  df6=data.frame(a=c(NA,2,NA,NA),b=c('Y',NA,NA,NA),c=c(NA,NA,FALSE,TRUE),stringsAsFactors = FALSE)
  df6original = data.frame(a=c(NA,2,NA),b=c('Y',NA,NA),c=c(NA,TRUE,FALSE),stringsAsFactors = FALSE)
  expect_identical(removeRowsWithNAInSpecCol(df6original,'b'), df6[1,])
})

test_that("Remove two rows wrong values", {
  df1=data.frame(a=c(NA,NA,2),b=c('N',NA,'Y'),c=c(FALSE,TRUE,NA),stringsAsFactors = TRUE)
  df1original = data.frame(a=c(NA,NA,3),b=c(NA,'N','Y'),c=c(FALSE,TRUE,NA),stringsAsFactors = TRUE)
  expect_false(identical(removeRowsWithNAInSpecCol(df1original,'a'), df1[3,]))

  df2=data.frame(a=c(1,NA,NA),b=c('N',NA,'Y'),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  df2original = data.frame(a=c(1,NA,3),b=c('Y',NA,NA),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_false(identical(removeRowsWithNAInSpecCol(df2original,'b'), df2[1,]))

  df3=data.frame(a=c(NA,NA,NA),b=c(NA,'Y',NA),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  df3original = data.frame(a=c(NA,2,NA),b=c('Y',NA,NA),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_false(identical(removeRowsWithNAInSpecCol(df3original,'b'), df3[1,]))

  df4=data.frame(a=c(2,NA,NA),b=c('Y',NA,NA),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  df4original = data.frame(a=c(NA,2,NA),b=c('Y',NA,NA),c=c(NA,TRUE,FALSE),stringsAsFactors = TRUE)
  expect_false(identical(removeRowsWithNAInSpecCol(df4original,'b'), df4[1,]))
})

test_that("Remove all rows successfully", {
  df1 = data.frame(a=logical(0),b=character(0),c=logical(0),stringsAsFactors = TRUE)
  df1original = data.frame(a=c(NA,NA,NA),b=c(NA,'Y','N'),c=c(TRUE,FALSE,NA),stringsAsFactors = TRUE)
  df1original<-removeRowsWithNAInSpecCol(df1original,'a')
  df1original$b<-factor(df1original$b)
  expect_identical(df1original,df1)

  df2 = data.frame(a=logical(0),b=character(0),c=logical(0),stringsAsFactors = FALSE)
  df2original = data.frame(a=c(NA,NA,NA),b=c(NA,'Y','N'),c=c(TRUE,FALSE,NA),stringsAsFactors = FALSE)
  df2original<-removeRowsWithNAInSpecCol(df2original,'a')
  expect_identical(df2original,df2)

  df3 = data.frame(a=numeric(0),b=logical(0),c=logical(0),stringsAsFactors = TRUE)
  df3original = data.frame(a=c(1,NA,3),b=c(NA,NA,NA),c=c(NA,FALSE,TRUE),stringsAsFactors = TRUE)
  df3original<-removeRowsWithNAInSpecCol(df3original,'b')
  expect_identical(df3original, df3)

  df4 = data.frame(a=numeric(0),b=logical(0),c=logical(0),stringsAsFactors = FALSE)
  df4original = data.frame(a=c(1,NA,3),b=c(NA,NA,NA),c=c(NA,FALSE,TRUE),stringsAsFactors = FALSE)
  df4original<-removeRowsWithNAInSpecCol(df4original,'b')
  expect_identical(df4original, df4)

  df5 = data.frame(a=numeric(0),b=character(0),c=logical(0),stringsAsFactors = TRUE)
  df5original = data.frame(a=c(1,NA,3),b=c(NA,'N','Y'),c=c(NA,NA,NA),stringsAsFactors = TRUE)
  df5original<-removeRowsWithNAInSpecCol(df5original,'c')
  df5original$b<-factor(df5original$b)
  expect_identical(df5original, df5)

  df6 = data.frame(a=numeric(0),b=character(0),c=logical(0),stringsAsFactors = FALSE)
  df6original = data.frame(a=c(1,NA,3),b=c(NA,'N','Y'),c=c(NA,NA,NA),stringsAsFactors = FALSE)
  df6original<-removeRowsWithNAInSpecCol(df6original,'c')
  expect_identical(df6original, df6)
})
