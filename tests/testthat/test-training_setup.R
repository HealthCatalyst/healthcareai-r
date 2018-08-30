# Test functions from R/training_setup.R
context("checking functions in training_setup.R")

test_that("check_outcome gets outcome from recipe when not provided", {
  d <-
    pima_diabetes %>%
    prep_data(patient_id, outcome = diabetes)

  # Pass an empty quosure to the outcome
  outcome <- check_outcome(rlang::enquo(), names(d), attr(d, "recipe"))
  expect_equal(outcome, "diabetes")
})
