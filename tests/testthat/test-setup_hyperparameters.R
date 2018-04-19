context("Checking hyperparameter set up")

test_that("get_hyperparameter_defaults returns one row data frame/s with right columns", {
  both_class <- get_hyperparameter_defaults(models = get_supported_models(),
                                            n = 10, k = 3, model_class = "classification")
  expect_equal(get_supported_models(), names(both_class))
  expect_true(all(purrr::map_lgl(both_class, is.data.frame)))
  expect_true(all(purrr::map_lgl(both_class, ~ nrow(.x) == 1)))
  expect_setequal(colnames(both_class$rf), c("mtry", "splitrule", "min.node.size"))
  expect_setequal(colnames(both_class$knn), c("kmax", "distance", "kernel"))
  rf_class <- get_hyperparameter_defaults(models = "rf", n = 10, k = 3,
                                          model_class = "classification")
  knn_class <- get_hyperparameter_defaults(models = "knn", n = 10, k = 3,
                                           model_class = "classification")
  expect_equal(both_class$rf, rf_class$rf)
  expect_equal(both_class$knn, knn_class$knn)

  both_reg <- get_hyperparameter_defaults(models = get_supported_models(),
                                          n = 1000, k = 40, model_class = "regression")
  expect_equal(get_supported_models(), names(both_reg))
  expect_true(all(purrr::map_lgl(both_reg, is.data.frame)))
  expect_true(all(purrr::map_lgl(both_reg, ~ nrow(.x) == 1)))
  expect_setequal(colnames(both_reg$rf), c("mtry", "splitrule", "min.node.size"))
  expect_setequal(colnames(both_reg$knn), c("kmax", "distance", "kernel"))
})

test_that("get_random_hyperparameters returns data frame/s with right columns", {
  both_class <- get_random_hyperparameters(models = get_supported_models(),
                                           n = 99, k = 18, tune_depth = 20,
                                           model_class = "classification")
  expect_equal(get_supported_models(), names(both_class))
  expect_true(all(purrr::map_lgl(both_class, is.data.frame)))
  expect_true(all(purrr::map_lgl(both_class, ~ nrow(.x) == 20)))
  expect_setequal(colnames(both_class$rf), c("mtry", "splitrule", "min.node.size"))
  expect_setequal(colnames(both_class$knn), c("kmax", "distance", "kernel"))
  rf_class <- get_random_hyperparameters(models = "rf", n = 66, k = 33,
                                         tune_depth = 20, model_class = "classification")
  knn_class <- get_random_hyperparameters(models = "knn", n = 66, k = 33,
                                          tune_depth = 20, model_class = "classification")
  expect_equal(get_classes_sorted(both_class$rf), get_classes_sorted(rf_class$rf))
  expect_equal(get_classes_sorted(both_class$knn), get_classes_sorted(knn_class$knn))


  both_reg <- get_random_hyperparameters(models = get_supported_models(),
                              n = 1000, k = 40, tune_depth = 12,
                              model_class = "regression")
  expect_equal(get_supported_models(), names(both_reg))
  expect_true(all(purrr::map_lgl(both_reg, is.data.frame)))
  expect_true(all(purrr::map_lgl(both_reg, ~ nrow(.x) == 12)))
  expect_setequal(colnames(both_reg$rf), c("mtry", "splitrule", "min.node.size"))
  expect_setequal(colnames(both_reg$knn), c("kmax", "distance", "kernel"))

  one_random_row <- get_random_hyperparameters(models = get_supported_models(),
                                               n = 1000, k = 40, tune_depth = 1,
                                               model_class = "regression")
  expect_setequal(colnames(both_reg$rf), colnames(one_random_row$rf))
  expect_true(all(purrr::map_lgl(one_random_row, ~ nrow(.x) == 1)))
})
