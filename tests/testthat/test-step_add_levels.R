context("Checking recipe step_add_levels")

# Setup ------------------------------------------------------------------------
d <- tibble(num = 1:30,
            has_missing = c(rep(NA, 10), rep("b", 20)),
            has_rare = c("rare", rep("common", 29)),
            has_both = c("rare", NA, rep("common", 28)),
            has_neither = c(rep("cat1", 15), rep("cat2", 15)))
d <- d %>% mutate(across(where(is.character), as.factor))
init <- recipe(~ ., d)
stepped <- step_add_levels(init, all_nominal())
prepped <- prep(stepped, training = d)
baked <- bake(prepped, d)

# Tests ------------------------------------------------------------------------
test_that("Recipe object is updated with step", {
  expect_s3_class(stepped$steps[[1]], "step_add_levels")
})

test_that("Recipe is prepped", {
  expect_s3_class(prepped$var_info, "data.frame")
  expect_true(all(c("levels", "terms") %in% names(prepped$steps[[1]])))
})

test_that("Recipe is baked correctly on training data", {
  expect_true(all(purrr::map_lgl(baked[, map_lgl(baked, is.factor)], ~
                       all(c("other", "missing") %in% levels(.x)))))
  expect_true(all.equal(d$num, baked$num))
})

test_that("Recipe is baked correctly on test data", {
  newd <- d[, sample(ncol(d))]
  newbake <- bake(prepped, newd)
  expect_true(all(purrr::map_lgl(newbake[, map_lgl(newbake, is.factor)], ~
                       all(c("other", "missing") %in% levels(.x)))))
})

test_that("Warning is triggered for greater than 50% NA", {
  unprepped_print <- capture_output(print(stepped))
  expect_true(any(map_lgl(unprepped_print, ~ grepl("Adding levels", .x))))
  expect_false(any(map_lgl(unprepped_print, ~ grepl("missing", .x))))
  prepped_print <- capture_output(print(prepped))
  expect_true(any(map_lgl(prepped_print, ~ grepl("Adding levels", .x))))
  expect_true(any(map_lgl(prepped_print, ~ grepl("missing", .x))))
})

test_that("tidy method prints correctly", {
  tidy_step <- tidy(stepped$steps[[1]])
  expect_s3_class(tidy_step, "tbl_df")
  expect_true(all.equal(names(tidy_step), c("terms", "value", "id")))
  tidy_prep <- tidy(prepped$steps[[1]])
  expect_s3_class(tidy_prep, "tbl_df")
  expect_true(all.equal(names(tidy_prep), c("terms", "value", "id")))
  expect_true(all.equal(tidy_prep$terms, names(d)[purrr::map_lgl(d, is.factor)]))
})
