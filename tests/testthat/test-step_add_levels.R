context("Checking recipe step_add_levels")

library(recipes)
# Setup ------------------------------------------------------------------------
d <- data.frame(num = 1:30,
                has_missing = c(rep(NA, 10), rep('b', 20)),
                has_rare = c("rare", rep("common", 29)),
                has_both = c("rare", NA, rep("common", 28)),
                has_neither = c(rep("cat1", 15), rep("cat2", 15)))
init <- recipe( ~ ., d)
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
                       all(c("other", "hcai_missing") %in% levels(.x)))))
  expect_true(all.equal(d$num, baked$num))
})

test_that("Recipe is baked correctly on test data", {
  newd <- d[, sample(ncol(d))]
  newbake <- bake(prepped, newd)
  expect_true(all(purrr::map_lgl(newbake[, map_lgl(newbake, is.factor)], ~
                       all(c("other", "hcai_missing") %in% levels(.x)))))
})

test_that("Printer method works correctly within print.recipe()", {
  expect_output(
    print(rec_obj),
    regexp = "[Filling NA with hcai_missing for character, suit]"
  )
})

test_that("Warning is triggered for greater than 50% NA", {
  expect_message(
   junk <- capture_output(
      junk <- prep(rec_obj2, training = d2_train)
   ),
  regexp = "[koopa: 61]"
  )
})

test_that("tidy method prints correctly", {
  exp <- tibble::tibble(terms = c("character", "suit"),
                value = c(33.20, 8.71))
  expect_equal(
    exp,
    broom::tidy(rec_obj$steps[[1]])
  )
})
