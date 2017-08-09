context('Checking that countMissingData is working')

test_that("For a given dataframe, function returns expected output", {
  
  dat <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                    b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                    c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                    d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  
  actualOut <- countMissingData(dat)
  
  expected <- c(0.500, 0.125, 0.375, 0.000)
  
  expect_equal(actualOut[[1]], expected[[1]])
  expect_equal(actualOut[[2]], expected[[2]])
  expect_equal(actualOut[[3]], expected[[3]])
})

test_that("For a given matrix, function returns expected output", {
   
  n <- matrix(c(1, 3, NA, NaN, "NULL", "NAs", "nil", "NONE"),
               nrow = 4, ncol = 2)
   
   actualOut <- countMissingData(n)
   
   expected <- c(0.5, 0.75)
   
   expect_equal(actualOut[[1]], expected[[1]])
   expect_equal(actualOut[[2]], expected[[2]])
})

test_that("For a given dataframe and user defined NA values, function returns
          expected output", {
  
  dat2 <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                    b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                    c = c(10, 5, 8, "void", NA, "NULL", NaN, "Nas"),
                    d = c(1, 6, 7, 8, 9, 5, 10, "what"))
  
  actualOut <- countMissingData(dat2, c("void", "what"))
  
  expected <- c(0.500, 0.125, 0.500, 0.125)
  
  expect_equal(actualOut[[2]], expected[[2]])
  expect_equal(actualOut[[4]], expected[[4]])
})

test_that("For a given vector, function returns expected error", {
   
  someRandomVector <- c(1, 2, 5, 6, 8, NA)
  
  expect_error(countMissingData(someRandomVector), 
               "countMissingData must be fed a matrix or a dataframe")
})

test_that("For a given data frame and bad class of user defined NAs, function
          returns expected error", {
  dat3 <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                     b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                     c = c(10, 5, 8, "void", NA, "NULL", NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, "what"))
  
  badNAs <- data.frame(a = c("what", "how"))
  
  expect_error(countMissingData(dat3, badNAs), 
               "User provided NAs must be in vector format")
})