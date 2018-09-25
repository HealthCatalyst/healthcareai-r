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

test_that("trunc_vars", {
  test_vec <- c("seven l", "eleven lets", "fifteen   letters")
  expect_equal(test_vec, trunc_char(test_vec, 18))
  expect_equal(trunc_char(test_vec, 14), c(test_vec[1:2], "fifte...tters"))
  expect_warning(short <- trunc_char(test_vec, 5), "5")
  expect_equal(short, c("seven", "eleve", "fifte"))
})

test_that("mode - test factor", {
  test_vec <- as.factor(c("a", "b", "c", "d", "b"))
  expect_equal(factor(c("b"), levels = c("a", "b", "c", "d")), Mode(test_vec))
})

test_that("mode - test character", {
  test_vec <- c("a", "b", "c", "d", "b")
  expect_equal("b", Mode(test_vec))
})

test_that("mode - test numeric", {
  test_vec <- c(3, 2, 1, 2, 3, 3)
  expect_equal(3, Mode(test_vec))
})

test_that("get_factor_levels", {
  fl <-
    dplyr::mutate(pima_diabetes, weight_class =
                    relevel(factor(weight_class), "underweight")) %>%
    get_factor_levels()
  expect_equal(names(fl$weight_class)[1], "underweight")
})

test_that("mode - test table", {
  test_vec <- c(3, 2, 1, 2, 3, 3)
  vec_ft <- table(test_vec)
  expect_equal(Mode(vec_ft), Mode(test_vec))

  test_vec <- c("a", "b", "c", "d", "b")
  vec_ft <- table(test_vec)
  expect_equal(Mode(vec_ft), Mode(test_vec))
})

test_that("get_recipe_step - test whether the step exists", {
  m <- machine_learn(pima_diabetes[1:50, ], outcome = diabetes)
  dummy_object <- get_recipe_step(m, "step_dummy_hcai")
  expect_true("step_dummy_hcai" %in% class(dummy_object))
  expect_true(is.null(get_recipe_step(m, "not_a_step")))
})
