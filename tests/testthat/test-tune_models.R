context("Checking model tuning")

# Setup ------------------------------------------------------------------------
set.seed(9346)
test_df <- data.frame(
  x1 = rep(letters[1:2], each = 10),
  x2 = c(rnorm(10, 5, 5), rnorm(10, 100, 5))
)
test_df$x3 <- as.integer(as.factor(test_df$x1)) * test_df$x2

## Temporary test until grid search is implemented
test_that("tune errors if tune_method isn't 'random'", {
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
test_that("tune doesn't error on knn regression", {
  expect_error(
    tune_models(d = test_df, outcome = x3, model_class = "regression",
         models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)

})

test_that("tune doesn't error on knn classification", {
  expect_error(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
         models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune doesn't error on rf regression", {
  expect_error(
    suppressWarnings(  # rf-regression issues unimportant warning sometimes
      tune_models(d = test_df, outcome = x3, model_class = "regression",
           models = "rf", n_folds = 2, tune_depth = 2)
    )
    , regexp = NA)
})

test_that("tune doesn't error on rf classification", {
  expect_error(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
         models = "rf", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune errors sensibly if outcome isn't present", {
  expect_error(tune_models(test_df, xxx), regexp = "xxx")
})

# Training multiple models in one call
test_that("tune doesn't error on rf & knn classification", {
  expect_error(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
         models = c("rf", "knn"), n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune returns a model_list of appropriate type", {
  c_models <-
    tune_models(d = test_df, outcome = x1, model_class = "classification",
         n_folds = 2, tune_depth = 2)
  # rf-regression issues unimportant warning sometimes
  suppressWarnings({
    r_models <-
      tune_models(d = test_df, outcome = x3, model_class = "regression",
           n_folds = 2, tune_depth = 2)
  })
  expect_s3_class(c_models, "model_list")
  expect_s3_class(c_models, "classification_list")
  expect_s3_class(r_models, "model_list")
  expect_s3_class(r_models, "regression_list")
})

test_that("tune returns a model_list of appropriate type when not specified", {
  c_models <-
    tune_models(d = test_df, outcome = x1, n_folds = 2, tune_depth = 2)
  # rf-regression issues unimportant warning sometimes
  suppressWarnings({
    r_models <-
      tune_models(d = test_df, outcome = x3, n_folds = 2, tune_depth = 2)
  })
  expect_s3_class(c_models, "model_list")
  expect_s3_class(c_models, "classification_list")
  expect_s3_class(r_models, "model_list")
  expect_s3_class(r_models, "regression_list")
})

test_that("tune errors informatively if outcome is list", {
  test_df$x3 <- as.list(test_df$x3)
  expect_error(
    tune_models(d = test_df, outcome = x3, n_folds = 2, tune_depth = 2),
    regexp = "list")
})

# Informative erroring
test_that("tune errors informatively if the algorithm isn't supported", {
  expect_error(tune_models(test_df, x3, "regression", "not a model"),
               regexp = "supported")
})

# Can handle various metrics. expect_warning because metric not found->default
test_that("tune supports various loss functions in classification", {
  expect_warning(
    tune_models(d = test_df, outcome = x1, model_class = "classification",
         metric = "ROC", models = "knn", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  # Not yet supported
  # expect_warning(
  #   tune_models(d = test_df, outcome = x1, model_class = "classification",
  #               metric = "mnLogLoss", models = "knn", n_folds = 2,
  #               tune_depth = 2)
  #   , regexp = NA)
  # expect_warning(
  #   tune_models(d = test_df, outcome = x1, model_class = "classification",
  #               metric = "PR", models = "knn", n_folds = 2, tune_depth = 2)
  #   , regexp = NA)
  # expect_warning(
  #   tune_models(d = test_df, outcome = x1, model_class = "classification",
  #               metric = "accuracy", models = "knn", n_folds = 2,
  #               tune_depth = 2)
  #   , regexp = NA)
})

test_that("tune supports various loss functions in regression", {
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

test_that("tune handles character outcome", {
  test_df$x1 <- as.character(test_df$x1)
  expect_s3_class(tune_models(test_df, x1, "classification"), "classification_list")
})

test_that("tune handles tibble input", {
  expect_s3_class(tune_models(tibble::as_tibble(test_df), x1, "classification"),
                  "classification_list")
})

test_that("If a column was ignored in prep_data it's ignored in tune", {
  pd <- prep_data(test_df, x3)
  mods <- tune_models(pd, x2)
  expect_false("x3" %in% names(mods[[1]]$trainingData))
})
