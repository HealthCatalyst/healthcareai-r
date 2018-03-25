context("checking untuned models")

dd <- na.omit(pima_diabetes)[51:100, ] %>% dplyr::select(-patient_id)
cl <- flash_models(dd, diabetes)
reg <- flash_models(dd, age)

test_that("flash_models returns appropriate model_list", {
  expect_s3_class(cl, "model_list")
  expect_s3_class(cl, "classification_list")
  expect_s3_class(reg, "model_list")
  expect_s3_class(reg, "regression_list")
})

test_that("flash_models let's user select model", {
  expect_error(flash_models(dd, diabetes, models = "rf"), NA)
  expect_error(flash_models(dd, diabetes, models = "knn"), NA)
})

test_that("flash_models errors informatively if hyperparameters and models don't match", {
  expect_error(flash_models(dd, diabetes, hyperparameters = get_hyperparameter_defaults("knn")),
               "knn")
  expect_error(flash_models(dd, diabetes, models = "rf",
                            hyperparameters = get_hyperparameter_defaults("knn")),
               "rf")
})

test_that("can predict on flash models", {
  predict(cl)
  predict(reg, dd[10:1, ])
})

test_that("model_list generics work with flash models", {
  expect_message(print(cl), "Best model")
  expect_message(summary(cl), "Best performance")
  expect_s3_class(plot(cl, print = FALSE), "gg")
})
