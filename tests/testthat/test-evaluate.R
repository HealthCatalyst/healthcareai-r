context("Checking evaluate")
library(magrittr)

# Setup
d <- na.omit(pima_diabetes)[1:100, ]
dtest <- na.omit(pima_diabetes)[101:110, ]

r_models <- machine_learn(d, patient_id, outcome = plasma_glucose, tune = FALSE, n_folds = 2)
c_models <- machine_learn(d, patient_id, outcome = diabetes, tune = FALSE, n_folds = 2)

r_preds_training <- predict(r_models)
r_preds_training_eval <- evaluate(r_preds_training)
c_preds_training <- predict(c_models)
c_preds_training_eval <- evaluate(c_preds_training)
r_preds_test_eval <- predict(r_models, dtest) %>% evaluate()
c_preds_test_eval <- predict(c_models, dtest) %>% evaluate()
all_preds <- list(r_preds_training, r_preds_test, c_preds_training, c_preds_test)

# Test
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

test_that("evaluate barfs if target isn't present", {
  expect_error(
    dplyr::select(dtest, -plasma_glucose) %>%
      predict(r_models, .) %>%
      evaluate()
    , regexp = "plasma_glucose")
  expect_error(
    dtest %>%
      predict(c_models, .) %>%
      dplyr::select(-diabetes) %>%
      evaluate()
    , regexp = "diabetes")
})

test_that("evaluate returns numeric vector", {
  purrr::map_lgl(all_preds, is.numeric) %>%
    all() %>%
    expect_true()
})

test_that("evalutes returns appropriate metrics", {
  expect_setequal(names(r_preds_training_eval), c("RMSE", "MAE", "Rsquared"))
  expect_setequal(names(r_preds_test_eval), c("RMSE", "MAE", "Rsquared"))
  expect_setequal(names(c_preds_training_eval), c("AUPR", "AUROC"))
  expect_setequal(names(c_preds_test_eval), c("AUPR", "AUROC"))
})

test_that("All AUx metrics are in [0, 1]", {
  map_lgl(c_preds_test_eval, ~ .x <= 1 && .x >= 0) %>%
    all() %>%
    expect_true()
})

test_that("evaluate metrics match caret's", {
  expect_equal(attributes(r_preds_training)$model_info$performance,
               r_preds_training_eval["RMSE"])
  expect_equal(attributes(c_preds_training)$model_info$performance,
               c_preds_training_eval["AUROC"])
})
