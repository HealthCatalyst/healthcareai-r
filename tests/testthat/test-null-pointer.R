context('Checking that nullPointer is working')

test_that("For a given dataframe, function returns expected output", {
  
  dat <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                    b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                    c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                    d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  
  actualOut <- nullPointer(dat)
  
  expected <- c(0.625, 0.500, 0.375)
  
  names(expected) <- c("a", "b", "c")
  
  expect_equal(actualOut[1], expected[1])
  expect_equal(actualOut[2], expected[2])
  expect_equal(actualOut[3], expected[3])
})


test_that("For a given matrix, function returns expected output", {
   
  n <- matrix(c(1, 3, NA, NaN, "NULL", "empty", "999", "NONE"),
               nrow = 4, ncol = 2)
   
   actualOut <- nullPointer(n)
   
   expected <- c(0.5, 1.0)
   
   names(expected) <- c("V1", "V2")
   
   expect_equal(actualOut[1], expected[1])
   expect_equal(actualOut[2], expected[2])
})


test_that("For a given vector, function returns expected error", {
   
  someRandomVector <- c(1, 2, 5, 6, 8, NA)
  
  expect_error(nullPointer(someRandomVector), 
               "nullPointer must be fed a matrix or a dataframe")
})


