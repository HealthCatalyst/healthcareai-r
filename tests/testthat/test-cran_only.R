context("CRAN tests")

test_that("the fundamentals work", {
  m <- machine_learn(pima_diabetes[1:100, ], patient_id, outcome = diabetes, tune = FALSE)
  expect_s3_class(m, "model_list")
  training_preds <- predict(m)
  expect_s3_class(training_preds, "predicted_df")
  expect_s3_class(training_preds, "data.frame")
  expect_true(is.numeric(training_preds$predicted_diabetes))
  test_pred_glm <- predict(m["glmnet"], pima_diabetes[101, ])
  expect_s3_class(test_pred_glm, "predicted_df")
  expect_true(is.numeric(test_pred_glm$predicted_diabetes))
})
