context("Checking evaluate")

# Setup
d <- na.omit(pima_diabetes)[1:100, ]
dtest <- na.omit(pima_diabetes)[101:110, ]

r_models <- machine_learn(d, patient_id, outcome = plasma_glucose, tune = FALSE, n_folds = 2)
c_models <- machine_learn(d, patient_id, outcome = diabetes, tune = FALSE, n_folds = 2)

r_preds_training <- predict(r_models)
c_preds_training <- predict(c_models)
r_preds_test <- predict(r_models, dtest)
c_preds_test <- predict(c_models, dtest)
all_preds <- list(r_preds_training, r_preds_test, c_preds_training, c_preds_test)

# Test
test_that("evaluate.hcai_predicted_df is registered generic", {
  expect_true(any(stringr::str_detect(methods("evaluate"), "hcai_predicted_df")))
})

test_that("evaluate returns numeric vector", {
  expect_true(all(map_lgl(all_preds, is.numeric)))
})

test_that("evalute_classification returns numeric with names being metrics", {
  eval_class <- evaluate_classification(c(.3, .7, .8), c(0, 1, 0))
  expect_true(is.numeric(eval_class))
  expect_setequal(names(eval_class), c("AUPR", "AUROC"))
})

test_that("evalute_regression returns numeric with names being metrics", {
  eval_reg <- evaluate_regression(c(.3, .7, .8), c(4, 6, 9))
  expect_true(is.numeric(eval_reg))
  expect_setequal(names(eval_reg), c("RMSE", "MAE", "Rsquared"))
})
