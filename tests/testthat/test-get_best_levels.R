context("Checking get_best_levels")

# Setup ----------------------------------------
set.seed(56073)
n <- 25
d <- tibble::tibble(
  patient_id = letters[1:n],
  x1 = rpois(n, 3),
  class_outcome = sample(c("N", "Y"), n, TRUE, c(5, 1)),
  reg_outcome = rnorm(n, 2, 10)
)
n2 <- 75
groups <- tibble::tibble(
  patient_id = sample(d$patient_id, n2, TRUE),
  grouper = sample(LETTERS[1:10], n2, TRUE)
)
# Make A drive high outcomes and J drive low outcomes as a check
d$class_outcome[d$patient_id %in% groups$patient_id[groups$grouper == "A"]] <- "Y"
d$reg_outcome[d$patient_id %in% groups$patient_id[groups$grouper == "A"]] <- quantile(d$reg_outcome, .95)
d$class_outcome[d$patient_id %in% groups$patient_id[groups$grouper == "J"]] <- "N"
d$reg_outcome[d$patient_id %in% groups$patient_id[groups$grouper == "J"]] <- quantile(d$reg_outcome, .05)

cbest3 <- get_best_levels(d, groups, patient_id, grouper, class_outcome, 3)
rbest4 <- get_best_levels(d, groups, patient_id, grouper, reg_outcome, 4)

# Tests ----------------------------------------
test_that("get_best_levels returns character vector of length n_levels", {
  expect_true(is.character(cbest3))
  expect_true(is.character(rbest4))
  expect_equal(3, length(cbest3))
  expect_equal(4, length(rbest4))
})

test_that("groups with really strong signal get pulled", {
  expect_true(all(c("A", "J") %in% cbest3))
  expect_true(all(c("A", "J") %in% rbest4))
})

test_that("get_best_levels works when n_levels is very small or long", {
  c1 <- get_best_levels(d, groups, patient_id, grouper, class_outcome, 1)
  r1 <- get_best_levels(d, groups, patient_id, grouper, reg_outcome, 1)
  expect_true(is.character(c1))
  expect_true(is.character(r1))
  expect_equal(1, length(c1))
  expect_equal(1, length(r1))

  c_all <- get_best_levels(d, groups, patient_id, grouper, class_outcome, 10)
  expect_equal(10, length(c_all))

  c_over <- get_best_levels(d, groups, patient_id, grouper, class_outcome, 100)
  expect_equal(c_all, c_over)
})

test_that("zip vectors works", {
  x <- 1:10
  y <- 11:15
  zipped <- zip_vectors(x, y)
  expect_true(is.numeric(zipped))
  expect_setequal(1:15, zipped)
  expect_setequal(c(1, 11), zipped[1:2])
  expect_equal(10, zipped[length(zipped)])
  mixed <- zip_vectors(x, letters)
  expect_true(is.character(mixed))
  expect_setequal(c("1", "a"), mixed[1:2])
  equal_length <- zip_vectors(letters, LETTERS)
  expect_equal(52, length(equal_length))
  expect_setequal(c(letters, LETTERS), equal_length)
  expect_equal(x, zip_vectors(numeric(), x))
})

test_that("add_best_levels returns a data frame with new columns", {
  added <- add_best_levels(d, groups, patient_id, grouper, class_outcome, 3)
  expect_s3_class(added, "tbl_df")
  expect_equal(3, sum(stringr::str_detect(names(added), "grouper_")))
})

test_that("add_best_levels adds all the columns if n_levels = Inf", {
  added <- add_best_levels(d, groups, patient_id, grouper, class_outcome, Inf)
  expect_setequal(unique(groups$grouper),
                  stringr::str_remove(stringr::str_subset(names(added), "^grouper"), "^grouper_"))
})

test_that("add_best_levels respects options passed to pivot", {
  groups$dose <- 2L
  added <- add_best_levels(d, groups, patient_id, grouper, reg_outcome, 4,
                           fill = dose, fun = prod, missing_fill = 0L)
  expect_equal(4, sum(stringr::str_detect(names(added), "grouper_")))
  expect_false(any(is.na(added)))
  expect_true(all(c(0, 2, 4) %in% added$grouper_F))
})

test_that("add_best_levels attaches levels as attribute", {
  added <- add_best_levels(d, groups, patient_id, grouper, class_outcome, 2)
  expect_true("grouper_levels" %in% names(attributes(added)))
  levs <- attr(added, "grouper_levels")
  expect_true(is.character(levs))
  expect_equal(2, length(levs))
})

test_that("add_best_levels adds empty columns if levels provided", {
  added <- add_best_levels(d, groups, patient_id, grouper, class_outcome, 2)
  test_row <- tibble::tibble(patient_id = "sam", x1 = 5)
  test_groups <- tibble::tibble(patient_id = rep("sam", 2), grouper = c("A", "new"))
  test_added <- add_best_levels(test_row, test_groups, patient_id, grouper,
                                levels = attr(added, "grouper_levels"))
  expect_false("grouper_new" %in% names(test_added))
  expect_equal(1, test_added$grouper_A)
  expect_true(is.na(test_added[[names(added)[ncol(added)]]]))
})

test_that("get_best_levels errors informatively if there's missingness in outcome", {
  d$reg_outcome[c(2, 12)] <- NA
  d$class_outcome[5] <- NA
  expect_error(get_best_levels(d, groups, patient_id, grouper, class_outcome, 5), "missingness")
  expect_error(get_best_levels(d, groups, patient_id, grouper, reg_outcome, 5), "missingness")
})

test_that("missingness in group doesn't affect get_best_levels", {
  minus1 <- add_best_levels(d, groups[-1, ], patient_id, grouper, class_outcome, 5)
  groups$grouper[1] <- NA
  missing1 <- add_best_levels(d, groups, patient_id, grouper, class_outcome, 5)
  expect_identical(minus1, missing1)
})

test_that("missingness in longsheet$id doesn't affect get_best_levels", {
  minus1 <- add_best_levels(d, groups[-3, ], patient_id, grouper, class_outcome, 5)
  groups$patient_id[3] <- NA
  missing1 <- add_best_levels(d, groups, patient_id, grouper, class_outcome, 5)
  expect_identical(minus1, missing1)
})

test_that("nothing filled for NA ID or ID not present in longsheet", {
  no_a <- add_best_levels(d, filter(groups, patient_id != "a"), patient_id, grouper, class_outcome, 5)
  expect_true(all(is.na(select(no_a, starts_with("grouper"))[1, ])))
  d$patient_id[1] <- NA
  missing_a <- add_best_levels(d, groups, patient_id, grouper, class_outcome, 5)
  expect_true(all(is.na(select(missing_a, starts_with("grouper"))[1, ])))
  expect_true(is.na(missing_a$patient_id[1]))
})
