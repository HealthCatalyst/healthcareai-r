context("Checking utility functions")

# Setup ------------------------------------------------------------------------
dd <- tibble::tibble(x = 1:10, y = c(NA, 9:1), z = c(letters[1:9], NA))

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
