
# Test functions from R/training_setup.R
context("checking functions in training_setup.R")

test_that("check_metric when correct, doesn't throw error or change metric", {
  expect_warning(metric <- check_metric("classification", "PR"), NA)
  expect_equal(metric, "PR")

  expect_warning(metric <- check_metric("regression", "RMSE"), NA)
  expect_equal(metric, "RMSE")

  expect_warning(metric <- check_metric("multiclass", "Kappa"), NA)
  expect_equal(metric, "Kappa")
})

test_that("check_metric when incorrect, throws error or throws warnings and corrects", {
  expect_warning(new_metric <- check_metric("classification", "MAE"))
  expect_equal(new_metric, "ROC")

  expect_warning(new_metric <- check_metric("regression", "PR"))
  expect_equal(new_metric, "RMSE")

  expect_warning(new_metric <- check_metric("multiclass", "MAE"))
  expect_equal(new_metric, "Accuracy")

  expect_error(check_metric("garbage", ""))
})

test_that("set_default_metric returns correct metric", {
  expect_equal(set_default_metric("regression"), "RMSE")
  expect_equal(set_default_metric("classification"), "ROC")

  expect_error(set_default_metric("garbage"), "Don't have")
})

test_that("check_outcome gets outcome from recipe when not provided", {
  d <-
    pima_diabetes %>%
    prep_data(patient_id, outcome = diabetes)

  # check_outcome should grab the outcome variable from the recipe object when
  # it is not provided
  outcome <- check_outcome(rlang::enquo(), names(d), attr(d, "recipe"))
  expect_equal(outcome, "diabetes")


  expect_error(
    check_outcome(rlang::enquo(), names(d), NULL),
    "Your data is not prepared."
  )
})
