context("Checking that all possible string combinations work ")

test_that("When one string is input, give correct list", {
  
  res <- createAllCombinations(c("one"))
  
  expected <- list()
  expected[[1]] <- "one"
  
  testthat::expect_equal(res,expected)
})

test_that("When two strings are input, give correct list", {
  
  res <- createAllCombinations(c("one","two"))
  
  expected <- list()          
  expected[[1]] <- "one"
  expected[[2]] <- "two"
  expected[[3]] <- c("one","two")
  
  testthat::expect_equal(res,expected)
})

test_that("When two numbers are input, give correct list", {
  
  res <- createAllCombinations(c(1,2))
  
  expected <- list()
  expected[[1]] <- 1
  expected[[2]] <- 2
  expected[[3]] <- c(1,2)
  
  testthat::expect_equal(res,expected)
})

test_that("When three strings are input, give correct list", {
  
  res <- createAllCombinations(c("one","two","three"))
  
  expected <- list()
  expected[[1]] <- "one"
  expected[[2]] <- "two"
  expected[[3]] <- c("one","two")
  expected[[4]] <- "three"
  expected[[5]] <- c("one","three")
  expected[[6]] <- c("two","three")
  expected[[7]] <- c("one","two","three")
  
  testthat::expect_equal(res,expected)
})