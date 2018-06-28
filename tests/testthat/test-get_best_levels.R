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
added <- add_best_levels(d, groups, patient_id, grouper, class_outcome, 2, missing_fill = 0L)
test_row <- tibble::tibble(patient_id = "sam", x1 = 5)
test_groups <- tibble::tibble(patient_id = rep("sam", 2), grouper = c("A", "new"))
test_added <- add_best_levels(test_row, test_groups, patient_id, grouper,
                              levels = attr(added, "best_levels"))
models <- list(
  fm = flash_models(dplyr::select(added, -patient_id), class_outcome, models = "xgb"),
  tm = tune_models(dplyr::select(added, -patient_id), class_outcome, models = "rf", tune_depth = 2),
  ml = machine_learn(added, patient_id, outcome = reg_outcome, tune = FALSE, tune_depth = 2)
)

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

test_that("min_obs is respected", {
  keepers <-
    groups %>%
    dplyr::group_by(grouper) %>%
    dplyr::summarize(n = n_distinct(patient_id)) %>%
    dplyr::filter(n >= 5) %>%
    nrow()
  expect_equal(keepers, length(get_best_levels(d, groups, patient_id, grouper, class_outcome, min_obs = 5)))
  expect_warning(x <- get_best_levels(d, groups, patient_id, grouper, class_outcome, min_obs = 100), "No levels")
  expect_equal(0, length(x))
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
  added_custom <- add_best_levels(d, groups, patient_id, grouper, reg_outcome, 4,
                           fill = dose, fun = prod, missing_fill = 0L)
  expect_equal(4, sum(stringr::str_detect(names(added_custom), "grouper_")))
  expect_false(any(is.na(added_custom)))
  expect_true(all(c(0, 2, 4) %in% added_custom$grouper_F))
})

test_that("add_best_levels attaches levels as attribute", {
  expect_true("grouper_levels" %in% names(attributes(added)$best_levels))
  levs <- attr(added, "best_levels")$grouper_levels
  expect_true(is.character(levs))
  expect_equal(2, length(levs))
})

test_that("add_best_levels adds empty columns if levels provided", {
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

test_that("add_best_levels adds multiple attributes to df if called multiple times", {
  added1 <- add_best_levels(d, groups, patient_id, grouper, class_outcome, 5)
  expect_true("grouper_levels" %in% names(attributes(added1)$best_levels))
  more_groups <- tibble::tibble(patient_id = rep(sample(d$patient_id, 3), 2),
                                newgroup = sample(letters[1:4], 6, TRUE))
  added2 <- add_best_levels(added1, more_groups, patient_id, newgroup, class_outcome, 2)
  expect_true("grouper_levels" %in% names(attributes(added2)$best_levels))
  expect_true("newgroup_levels" %in% names(attributes(added2)$best_levels))
})

test_that("get_best_levels works if all groups have same predictive potential", {
  same_outcome <- d$patient_id[d$class_outcome == "Y"][1:2]
  g <- expand.grid(patient_id = same_outcome, groups = c("A", "B"), stringsAsFactors = FALSE)
  expect_setequal(c("A", "B"), get_best_levels(d, g, patient_id, groups, class_outcome))
})

test_that("add_best_levels can pull X_levels from base", {
  test_added_easy <- add_best_levels(test_row, test_groups, patient_id, grouper, levels = added)
  expect_identical(test_added, test_added_easy)
})

test_that("add_best_levels can pull X_levels from best_levels list", {
  test_added_list <- add_best_levels(test_row, test_groups, patient_id, grouper,
                                     levels = attr(added, "best_levels"))
  expect_identical(test_added, test_added_list)
})

test_that("model_lists get X_levels attributes from input data frame", {
  purrr::map_lgl(models, ~ all.equal(attr(.x, "best_levels"), attr(added, "best_levels"))) %>%
    all() %>%
    expect_true()
})

test_that("add_best_levels can pull X_levels from a model_list object", {
  test_added_model1 <- add_best_levels(test_row, test_groups, patient_id, grouper, levels = models$tm)
  test_added_model2 <- add_best_levels(test_row, test_groups, patient_id, grouper, levels = models$ml)
  expect_identical(test_added, test_added_model1)
  expect_identical(test_added, test_added_model2)
})

test_that("Informative error if there are multiple observations per obs in d", {
   dd <- dplyr::bind_rows(d[1, ], d)
   expect_error(get_best_levels(dd, groups, patient_id, grouper, class_outcome, 3),
                dd$patient_id[1])
})

test_that("d and longsheet can be the same table", {
  both <-
    groups %>%
    group_by(patient_id) %>%
    sample_n(1) %>%
    left_join(d, ., by = "patient_id")
  expect_s3_class(add_best_levels(both, both, patient_id, grouper, class_outcome, 5),
                  "data.frame")
})
