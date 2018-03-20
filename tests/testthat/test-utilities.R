context("Checking test-utilities.R")

# Setup ------------------------------------------------------------------------
dd <- tibble::tibble(x = 1:10, y = c(NA, 9:1), z = c(letters[1:9], NA))
ddd <- tibble::tibble(x = 15:24, y = 0, z = c("a", rep("b", 9)))

# Test missing_check -----------------------------------------------------------
test_that("missing_check stops if there's missingness in quo'd variable", {
  expect_error(missing_check(dd, dplyr::quo(y)), "missingness")
  expect_error(missing_check(dd, dplyr::quo(z)), "missingness")
})

test_that("missing_check stops if there's missingness in string variable", {
  expect_error(missing_check(dd, "y"), "missingness")
  expect_error(missing_check(dd, "z"), "missingness")
})

test_that("missing_check returns TRUE if no missingness in quo'd var", {
  expect_true(missing_check(dd, dplyr::quo(x)))
})

test_that("missing_check returns TRUE if there isn't missingness in str var", {
  expect_true(missing_check(dd, "x"))
})

# Test find_new_levels ---------------------------------------------------------
test_that("find_new_levels works as expected", {
  dd <- tidyr::replace_na(dd, list(y = 0, z = "a"))
  expect_true(is.list(find_new_levels(dd, ddd)))
  expect_equal(1, length(find_new_levels(dd, ddd)))
  expect_equal(0, length(find_new_levels(ddd, dd)$z))
  expect_equal(letters[3:9], find_new_levels(dd, ddd)$z)
})

test_that("find_new_levels finds NAs", {
  expect_equal(1, sum(is.na(find_new_levels(dd, ddd)$z)))
  expect_false(any(is.na(find_new_levels(ddd, dd)$z)))
})

test_that("new columns are ignored by find_new_levels", {
  d1 <- data.frame(x = c("a", "b"))
  d2 <- data.frame(x = c("a", "b"), y = c("c", "d"))
  expect_false("y" %in% names(find_new_levels(d2, d1)))
})
