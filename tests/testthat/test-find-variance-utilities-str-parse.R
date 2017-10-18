context("Checking findVariance pipe-delimited string parsing functions")

test_that("Two words pipe-delimited count gives correct result", {

  testthat::expect_equal(getPipedWordCount("hello|sir"),
                         2)
})

test_that("Three regular words pipe-delimited count gives correct result", {
  
  testthat::expect_equal(getPipedWordCount("hello|sir|welcome"),
                         3)
})

test_that("Three one-letter words pipe-delimited count gives correct result", {
  
  testthat::expect_equal(getPipedWordCount("h|r|w"),
                         3)
})


test_that("Grab number after lowercase word gives correct result", {

  testthat::expect_equal(getPipedValue("hello|45"),
                         45)
})

test_that("Grab number after uppercase word gives correct result", {
  
  testthat::expect_equal(getPipedValue("JIMMY|46"),
                         46)
})

test_that("Grab number with multiple pipes gives correct result", {
  
  testthat::expect_equal(getPipedValue("JIMMY|47|WORD"),
                         47)
})

test_that("Grab number after one-letter word gives correct result", {
  
  testthat::expect_equal(getPipedValue("Y|47"),
                         47)
})

test_that("When no | is present in input, give correct error", {
  
  testthat::expect_error(getPipedValue("LOS"),
                         paste0("Your input string doesn't contain either a |,",
                                " a number, or both"),
                         fixed = TRUE)
})

test_that("When no number is input after |, give correct error", {
  
  testthat::expect_error(getPipedValue("LOS|"),
                         paste0("Your input string doesn't contain either a |,",
                                " a number, or both"),
                         fixed = TRUE)
})