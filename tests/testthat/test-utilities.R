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

test_that("find_new_levels works with list of levels from get_factor_levels", {
  a <- find_new_levels(dd, ddd)
  b <- find_new_levels(get_factor_levels(dd), ddd)
  c <- find_new_levels(dd, get_factor_levels(ddd))
  expect_equal(a, b)
  expect_equal(a, c)
})

test_that("format_new_levels respects remove_nas", {
  with_na <- find_new_levels(dd, ddd)
  expect_true(grepl("NA", format_new_levels(with_na, remove_nas = FALSE)))
  expect_false(grepl("NA", format_new_levels(with_na, remove_nas = TRUE)))
})

test_that("dfs_compatible works", {
  expect_true(dfs_compatible(dd, ddd))
  d1 <- dd %>% dplyr::mutate(x = sample(letters, nrow(dd)))
  expect_false(dfs_compatible(d1, ddd))
  expect_false(dfs_compatible(ddd, d1))
  d2 <- dd %>% dplyr::select(x, y)
  expect_false(dfs_compatible(ddd, d2))
  expect_true(dfs_compatible(d2, ddd))
})

test_that("select_not works and returns a data frame in all cases", {
  d <- data.frame(x = 1:5, y = rnorm(5), z = letters[1:5])
  expect_setequal(names(select_not(d, "x")), c("y", "z"))
  expect_equal(select_not(d, "y"), select_not(d, rlang::quo(y)))
  d <- select_not(d, "x")
  expect_s3_class(d <- select_not(d, rlang::quo(y)), "data.frame")
  expect_s3_class(select_not(d, "z"), "data.frame")
})
