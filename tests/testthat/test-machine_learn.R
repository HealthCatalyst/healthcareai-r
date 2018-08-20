context("Test machine_learn")

set.seed(257056)
training_data <- dplyr::sample_n(pima_diabetes, 25)
models <- machine_learn(training_data,
                        outcome = diabetes, models = "xgb", tune = FALSE)

test_that("machine_learn produces a model_list", {
  expect_s3_class(models, "model_list")
})

test_that("Can predict on output of machine_learn", {
  suppressWarnings({
    preds <- predict(models, pima_diabetes[1:5, ])
  })
  expect_s3_class(preds, "predicted_df")
  expect_true(all(c("diabetes", "predicted_diabetes") %in% names(preds)))
})

test_that("Get an informative error message for missing / wrong-class args", {
  expect_error(machine_learn(), "missing")
  expect_error(machine_learn(1), "data frame")
  expect_error(machine_learn(training_data), "outcome")
  expect_error(machine_learn(training_data, outcome = 1), "outcome")
  expect_error(machine_learn(training_data, outcome = am, not_there),
               "were passed to the ... argument")
})

test_that("Machine learn points the user to naming outcome if unprovided", {
  expect_error(machine_learn(pima_diabetes, diabetes), "outcome = ")
})

test_that("Machine learn respects CV details", {
  m <- machine_learn(training_data, outcome = diabetes, n_folds = 2, tune_depth = 3, models = "rf")
  expect_equal(2, m[[1]]$control$number)
  expect_equal(3, nrow(m[[1]]$results))
})

test_that("Machine learn respects tune = FALSE", {
  ut <- machine_learn(training_data, outcome = pedigree, n_folds = 3,
                      tune = FALSE, models = "xgb")
  expect_false(attr(ut, "tuned"))
})

test_that("Machine learn respects metric - on tune_models", {
  mock_tune_models <- function(d,
                               outcome,
                               models,
                               metric,
                               positive_class,
                               n_folds = 5,
                               tune_depth = 10,
                               hyperparameters = NULL,
                               model_class,
                               model_name = NULL,
                               allow_parallel = FALSE) {return(metric)}

  with_mock(tune_models = mock_tune_models, {
    mdl_metric <- machine_learn(training_data, outcome = diabetes,
                                metric = "PR")
    expect_equal(mdl_metric, "PR")
    mdl_metric <- machine_learn(training_data, outcome = age,
                                metric = "RMSE")
    expect_equal(mdl_metric, "RMSE")
  })
})

test_that("Machine learn respects metric - on flash_models", {

  mock_flash_models <- function(d,
                                outcome,
                                models,
                                metric,
                                positive_class,
                                n_folds = 5,
                                model_class,
                                model_name = NULL,
                                allow_parallel = FALSE) {return(metric)}

  with_mock(flash_models = mock_flash_models, {
    mdl_metric <- machine_learn(training_data, outcome = diabetes, tune = FALSE,
                                metric = "PR")
    expect_equal(mdl_metric, "PR")
    mdl_metric <- machine_learn(training_data, outcome = age, tune = FALSE,
                                metric = "RMSE")
    expect_equal(mdl_metric, "RMSE")
  })
})
