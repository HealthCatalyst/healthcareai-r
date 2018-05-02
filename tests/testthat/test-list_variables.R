context("Testing list_variables")

# Setup ------------------------------------------------------------------------
a <- "First Variable"
b <- "Second Variable"
c <- "Third Variable"
d <- "Forth Variable"
e <- "Fifth Variable"

# Tests ------------------------------------------------------------------------
test_that("Test error on empty vector", {
  x <- vector(mode = "character", length = 0)
  expect_error(list_variables(x), "vector_length_0")
})

test_that("Test structure of many variables", {
  expect_equal(list_variables(c(a, b, c, d, e, c, a, b, c, d)), "First Variable, Second Variable, Third Variable, Forth Variable, and Fifth Variable")
})

test_that("Test that output is two variablew with no commas", {
  expect_equal(list_variables(c(a, b, a, b, b, a)), "First Variable and Second Variable")
})

test_that("Test that output is one variable", {
  expect_equal(list_variables(c(c, c, c, c, c, c, c)), "Third Variable")
})

test_that("Test that output works on factors", {
  expect_equal(list_variables(factor(letters[1:3])), "a, b, and c")
})

test_that("Test error on non-equivalent Types in list", {
  expect_error(list_variables(list(x = list(a = 1, b = 2), y = 3)), "not_atomic")
})
