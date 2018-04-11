context("Checking evaluate")
library(magrittr)

# Setup
d <- na.omit(pima_diabetes)[1:100, ]
dtest <- na.omit(pima_diabetes)[101:110, ]

r_models <- machine_learn(d, patient_id, outcome = plasma_glucose, tune = FALSE, n_folds = 2)
r_models_eval <- evaluate(r_models)
c_models <- machine_learn(d, patient_id, outcome = diabetes, tune = FALSE, n_folds = 2)
c_models_eval <- evaluate(c_models)

r_preds_training <- predict(r_models)
r_preds_train_eval <- evaluate(r_preds_training)
r_preds_test_eval <- predict(r_models, dtest) %>% evaluate()
c_preds_training <- predict(c_models)
c_preds_train_eval <- evaluate(c_preds_training)
c_preds_test_eval <- predict(c_models, dtest) %>% evaluate()

all_evals <- list(r_models_eval, c_models_eval,
                  r_preds_train_eval, c_preds_train_eval,
                  r_preds_test_eval, c_preds_test_eval)

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

test_that("evaluate is a registered S3 generic with methods for models and predictions", {
  expect_true("evaluate.predicted_df" %in% methods("evaluate"))
  expect_true("evaluate.model_list" %in% methods("evaluate"))
})

test_that("All evaluate methods return numeric vector", {
  purrr::map_lgl(all_evals, is.numeric) %>%
    all() %>%
    expect_true()
})

test_that("All evalutes methods returns appropriate metrics", {
  expect_setequal(names(r_preds_train_eval), c("RMSE", "MAE", "Rsquared"))
  expect_setequal(names(r_preds_test_eval), c("RMSE", "MAE", "Rsquared"))
  expect_setequal(names(r_models_eval), c("RMSE", "MAE", "Rsquared"))
  expect_setequal(names(c_preds_train_eval), c("AUPR", "AUROC"))
  expect_setequal(names(c_preds_test_eval), c("AUPR", "AUROC"))
  expect_setequal(names(c_models_eval), c("AUPR", "AUROC"))
})

test_that("All classification metrics are in [0, 1]", {
  purrr::map_lgl(c_preds_test_eval, ~ .x <= 1 && .x >= 0) %>%
    all() %>%
    expect_true()
  purrr::map_lgl(c_models_eval, ~ .x <= 1 && .x >= 0) %>%
    all() %>%
    expect_true()
})

test_that("evaluate predicted barfs if target isn't present", {
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

test_that("evaluate model_list metrics match caret's", {
  expect_equal(attributes(r_preds_training)$model_info$performance,
               r_models_eval[["RMSE"]])
  expect_equal(attributes(c_preds_training)$model_info$performance,
               c_models_eval[["AUROC"]])
})
