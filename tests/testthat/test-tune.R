context("Checking model tuning")

# Setup ------------------------------------------------------------------------
set.seed(9346)
test_df <- data.frame(
  x1 = rep(letters[1:2], each = 10),
  x2 = rnorm(20)
)
test_df$x3 <- as.integer(as.factor(test_df$x1)) * test_df$x2

## Temporary test until grid search is implemented
test_that("tune_models errors if tune_method isn't 'random'", {
  expect_error(tune_models(test_df, x3, "regression", tune_method = "grid"),
               "random")
})

test_that("Error informatively if outcome class doesn't match model_class", {
  expect_error(tune_models(test_df, x1, "regression"), class(test_df$x1))
  test_df$x1 <- as.character(test_df$x1)
  expect_error(tune_models(test_df, x1, "regression"), class(test_df$x1))
  expect_error(tune_models(test_df, x3, "classification"), class(test_df$x3))
})

# No error for each algorithm x response-class type
test_that("tune_models doesn't error on knn regression", {
  expect_error(
    tune_models(d = test_df, outcome = x3, model_class = "regression",
                models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)

})

test_that("tune_models doesn't error on knn classification", {
  expect_error(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
                models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune_models doesn't error on rf regression", {
  expect_error(
    tune_models(d = test_df, outcome = x3, model_class = "regression",
                models = "rf", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune_models doesn't error on rf classification", {
  expect_error(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
                models = "rf", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

# Training multiple models in one call
test_that("tune_models doesn't error on rf & knn classification", {
  expect_error(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
                models = c("rf", "knn"), n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune_models returns a model_list", {
  c_models <-
    tune_models(d = test_df, outcome = x1, model_class = "classification",
                n_folds = 2, tune_depth = 2)
  r_models <-
    tune_models(d = test_df, outcome = x3, model_class = "regression",
                n_folds = 2, tune_depth = 2)
  expect_s3_class(c_models, "model_list")
  expect_s3_class(c_models, "classification_list")
  expect_s3_class(r_models, "model_list")
  expect_s3_class(r_models, "regression_list")
})

# Informative erroring
test_that("tune_models errors informatively if the algorithm isn't supported", {
  expect_error(tune_models(test_df, x3, "regression", "not a model"),
               regexp = "supported")
})

# Can handle various metrics
test_that("tune_models supports various loss functions in classification", {
  expect_warning(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
                metric = "AUROC", models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  expect_warning(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
                metric = "mnLogLoss", models = "knn", n_folds = 2,
                tune_depth = 2)
    , regexp = NA)
  expect_warning(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
                metric = "PR", models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  expect_warning(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
                metric = "accuracy", models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune_models supports various loss functions in regression", {
  expect_warning(
    tune_models(d = test_df, outcome = x2, model_class = "regression",
                metric = "MAE", models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  expect_warning(
    tune_models(d = test_df, outcome = x2, model_class = "regression",
                metric = "Rsquared", models = "knn", n_folds = 2,
                tune_depth = 2)
    , regexp = NA)
})
