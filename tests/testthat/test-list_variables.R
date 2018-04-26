context("Testing list_variables")

# Setup ------------------------------------------------------------------------
a <- "First Variable"
b <- "Second Variable"
c <- "Third Variable"
d <- "Forth Variable"
e <- "Fifth Variable"

# Tests ------------------------------------------------------------------------
test_that("Make sure input is a vector", {
  expect_error(list_variables(c()), "not_vector")
})

test_that("Test error on empty vector", {
  x <- vector(mode = "character", length = 0)
  expect_error(list_variables(x), "vector_length_0")
})

test_that("Test error on empty vector", {
  expect_equal(list_variables(c(a, b, c, d, e, c, a, b, c, d)), "First Variable, Second Variable, Third Variable, Forth Variable, and Fifth Variable")
})

test_that("Test error on empty vector", {
  expect_equal(list_variables(c(a, b, a, b, b, a)), "First Variable and Second Variable")
})

test_that("Test error on empty vector", {
  expect_equal(list_variables(c(c, c, c, c, c, c, c)), "Third Variable")
})
