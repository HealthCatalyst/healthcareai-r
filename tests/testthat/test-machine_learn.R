context("Test machine_learn")

test_data <- mtcars[1:5, ]
training_data <- mtcars[6:32, ]
models <- machine_learn(training_data, outcome = am)

test_that("machine_learn produces a model_list", {
  expect_s3_class(models, "model_list")
})

test_that("Can predict on output of machine_learn", {
  suppressWarnings({
    preds <- predict(models, test_data)
  })
  expect_s3_class(preds, "hcai_predicted_df")
  expect_true(all(c("am", "predicted_am") %in% names(preds)))
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
  expect_error(machine_learn(training_data, am), "outcome = ")
})
