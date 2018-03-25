context("test-tune_models")

# Setup ------------------------------------------------------------------------
test_df <- na.omit(pima_diabetes)[1:200, ]

## Temporary test until grid search is implemented
test_that("tune errors if tune_method isn't 'random'", {
  expect_error(tune_models(test_df, plasma_glucose, "regression", tune_method = "grid"),
               "random")
})

test_that("Error informatively if outcome class doesn't match model_class", {
  expect_error(tune_models(test_df, diabetes, "regression"), "categorical")
  test_df$diabetes <- factor(test_df$diabetes)
  expect_error(tune_models(test_df, diabetes, "regression"), "categorical")
  expect_error(tune_models(test_df, plasma_glucose, "classification"), "numeric")
})

# No error for each algorithm x response-class type
test_that("tune doesn't error on knn regression", {
  expect_error(
    tune_models(d = test_df, outcome = plasma_glucose, model_class = "regression",
         models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)

})

test_that("tune doesn't error on knn classification", {
  expect_error(
    tune_models(d = test_df, outcome = diabetes, model_class = "classification",
         models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune doesn't error on rf regression", {
  expect_error(
      tune_models(d = test_df, outcome = plasma_glucose, model_class = "regression",
           models = "rf", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune doesn't error on rf classification", {
  expect_error(
    tune_models(d = test_df, outcome = diabetes, model_class = "classification",
         models = "rf", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune errors sensibly if outcome isn't present", {
  expect_error(tune_models(test_df, xxx), regexp = "xxx")
})

# Training multiple models in one call
test_that("tune doesn't error on rf & knn classification", {
  expect_error(
    tune_models(d = test_df, outcome = diabetes, model_class = "classification",
         models = c("rf", "knn"), n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune returns a model_list of appropriate type", {
  c_models <-
    tune_models(d = test_df, outcome = diabetes, model_class = "classification",
         n_folds = 2, tune_depth = 2)
    r_models <-
      tune_models(d = test_df, outcome = plasma_glucose, model_class = "regression",
           n_folds = 2, tune_depth = 2)
  expect_s3_class(c_models, "model_list")
  expect_s3_class(c_models, "classification_list")
  expect_s3_class(r_models, "model_list")
  expect_s3_class(r_models, "regression_list")
})

test_that("tune returns a model_list of appropriate type when not specified", {
  c_models <-
    tune_models(d = test_df, outcome = diabetes, n_folds = 2, tune_depth = 2)
    r_models <-
      tune_models(d = test_df, outcome = plasma_glucose, n_folds = 2, tune_depth = 2)
  expect_s3_class(c_models, "model_list")
  expect_s3_class(c_models, "classification_list")
  expect_s3_class(r_models, "model_list")
  expect_s3_class(r_models, "regression_list")
})

test_that("tune errors informatively if outcome is list", {
  test_df$plasma_glucose <- as.list(test_df$plasma_glucose)
  expect_error(
    tune_models(d = test_df, outcome = plasma_glucose, n_folds = 2, tune_depth = 2),
    regexp = "list")
})

# Informative erroring
test_that("tune errors informatively if the algorithm isn't supported", {
  expect_error(tune_models(test_df, plasma_glucose, "regression", "not a model"),
               regexp = "supported")
})

# Can handle various metrics. expect_warning because metric not found->default
test_that("tune supports various loss functions in classification", {
  expect_warning(
    tune_models(d = test_df, outcome = diabetes, model_class = "classification",
         metric = "ROC", models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  # Not yet supported
  # expect_warning(
  #   tune_models(d = test_df, outcome = diabetes, model_class = "classification",
  #               metric = "mnLogLoss", models = "knn", n_folds = 2,
  #               tune_depth = 2)
  #   , regexp = NA)
  expect_warning(
    tune_models(d = test_df, outcome = diabetes, model_class = "classification",
                metric = "PR", models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  # expect_warning(
  #   tune_models(d = test_df, outcome = diabetes, model_class = "classification",
  #               metric = "accuracy", models = "knn", n_folds = 2,
  #               tune_depth = 2)
  #   , regexp = NA)
})

test_that("tune supports various loss functions in regression", {
  expect_warning(
    tune_models(d = test_df, outcome = age, model_class = "regression",
         metric = "MAE", models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  expect_warning(
    tune_models(d = test_df, outcome = age, model_class = "regression",
         metric = "Rsquared", models = "knn", n_folds = 2,
         tune_depth = 2)
    , regexp = NA)
})

test_that("tune handles character outcome", {
  test_df$diabetes <- as.character(test_df$diabetes)
  expect_s3_class(tune_models(test_df, diabetes, "classification"), "classification_list")
})

test_that("tune handles tibble input", {
  expect_s3_class(tune_models(tibble::as_tibble(test_df), diabetes, "classification"),
                  "classification_list")
})

test_that("If a column was ignored in prep_data it's ignored in tune", {
  pd <- prep_data(test_df, plasma_glucose)
  capture_warnings(mods <- tune_models(pd, age))
  expect_false("plasma_glucose" %in% names(mods[[1]]$trainingData))
})

test_that("Missing outcome variable error points user to what's missing", {
  expect_error(tune_models(test_df), "outcome")
})
