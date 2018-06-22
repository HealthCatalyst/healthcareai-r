context("Checking hyperparameter set up")

test_that("get_hyperparameter_defaults returns one row data frame/s with right columns", {
  all_class <- get_hyperparameter_defaults(models = get_supported_models(),
                                            n = 10, k = 3, model_class = "classification")
  expect_equal(get_supported_models(), names(all_class))
  expect_true(all(purrr::map_lgl(all_class, is.data.frame)))
  expect_true(all(purrr::map_lgl(all_class, ~ nrow(.x) %in% c(1, 10))))
  expect_setequal(colnames(all_class$rf), c("mtry", "splitrule", "min.node.size"))
  expect_setequal(colnames(all_class$xgb), c("nrounds", "max_depth", "eta", "gamma",
                                             "colsample_bytree", "min_child_weight", "subsample"))
  expect_setequal(colnames(all_class$glm), c("alpha", "lambda"))
  rf_class <- get_hyperparameter_defaults(models = "rf", n = 10, k = 3,
                                          model_class = "classification")
  xgb_class <- get_hyperparameter_defaults(models = "xgb", n = 10, k = 3,
                                           model_class = "classification")
  glm_class <- get_hyperparameter_defaults(models = "glm", n = 10, k = 3,
                                           model_class = "classification")
  expect_equal(all_class$rf, rf_class$rf)
  expect_equal(all_class$xgb, xgb_class$xgb)
  expect_equal(all_class$glm, glm_class$glm)

  all_reg <- get_hyperparameter_defaults(models = get_supported_models(),
                                          n = 1000, k = 40, model_class = "regression")
  expect_equal(get_supported_models(), names(all_reg))
  expect_true(all(purrr::map_lgl(all_reg, is.data.frame)))
  expect_true(all(purrr::map_lgl(all_reg, ~ nrow(.x) %in% c(1, 10))))
  expect_setequal(colnames(all_reg$rf), c("mtry", "splitrule", "min.node.size"))
  expect_setequal(colnames(all_reg$xgb), c("nrounds", "max_depth", "eta", "gamma",
                                           "colsample_bytree", "min_child_weight", "subsample"))
  expect_setequal(colnames(all_reg$glm), c("alpha", "lambda"))
})

test_that("get_random_hyperparameters returns data frame/s with right columns", {
  all_class <- get_random_hyperparameters(models = get_supported_models(),
                                           n = 99, k = 18, tune_depth = 20,
                                           model_class = "classification")
  expect_equal(get_supported_models(), names(all_class))
  expect_true(all(purrr::map_lgl(all_class, is.data.frame)))
  expect_true(all(purrr::map_lgl(all_class, ~ nrow(.x) %in% c(20, 40))))
  expect_setequal(colnames(all_class$rf), c("mtry", "splitrule", "min.node.size"))
  expect_setequal(colnames(all_class$xgb), c("nrounds", "max_depth", "eta", "gamma",
                                             "colsample_bytree", "min_child_weight", "subsample"))
  expect_setequal(colnames(all_class$glm), c("alpha", "lambda"))
  rf_class <- get_random_hyperparameters(models = "rf", n = 66, k = 33,
                                         tune_depth = 20, model_class = "classification")
  xgb_class <- get_random_hyperparameters(models = "xgb", n = 66, k = 33,
                                          tune_depth = 20, model_class = "classification")
  glm_class <- get_random_hyperparameters(models = "glm", n = 66, k = 33,
                                          tune_depth = 20, model_class = "classification")
  expect_equal(get_classes_sorted(all_class$rf), get_classes_sorted(rf_class$rf))
  expect_equal(get_classes_sorted(all_class$xgb), get_classes_sorted(xgb_class$xgb))
  expect_equal(get_classes_sorted(all_class$glm), get_classes_sorted(glm_class$glm))


  all_reg <- get_random_hyperparameters(models = get_supported_models(),
                              n = 1000, k = 40, tune_depth = 12,
                              model_class = "regression")
  expect_equal(get_supported_models(), names(all_reg))
  expect_true(all(purrr::map_lgl(all_reg, is.data.frame)))
  expect_true(all(purrr::map_lgl(all_reg, ~ nrow(.x) %in% c(12, 24))))
  expect_setequal(colnames(all_reg$rf), c("mtry", "splitrule", "min.node.size"))
  expect_setequal(colnames(all_reg$xgb), c("nrounds", "max_depth", "eta", "gamma",
                                           "colsample_bytree", "min_child_weight", "subsample"))
  expect_setequal(colnames(all_reg$glm), c("alpha", "lambda"))

  one_random_row <- get_random_hyperparameters(models = get_supported_models(),
                                               n = 1000, k = 40, tune_depth = 1,
                                               model_class = "regression")
  expect_setequal(colnames(all_reg$rf), colnames(one_random_row$rf))
  expect_true(all(purrr::map_lgl(one_random_row, ~ nrow(.x) %in% c(1, 2))))
})
