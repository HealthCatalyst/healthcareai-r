context("checking untuned models")

set.seed(2057)
dd <- na.omit(pima_diabetes)[51:100, ]
cl_prep <- prep_data(dd, patient_id, outcome = diabetes)
cl <- flash_models(cl_prep, diabetes, models = "xgb")
reg <- dd %>%
  prep_data(patient_id, outcome = age) %>%
  flash_models(age, models = "glm")

td <- na.omit(pima_diabetes)[1:40, ]
reg_df <- prep_data(td, patient_id, outcome = plasma_glucose)
cla_df <- prep_data(td, patient_id, outcome = diabetes)

test_that("flash_models returns appropriate model_list", {
  expect_s3_class(cl, "model_list")
  expect_s3_class(cl, "classification_list")
  expect_s3_class(reg, "model_list")
  expect_s3_class(reg, "regression_list")
})

test_that("flash_models let's user select model", {
  expect_error(flash_models(cl_prep, diabetes, models = "rf"), NA)
  expect_error(flash_models(cl_prep, diabetes, models = "glm"), NA)
})

test_that("flash_models are model_lists with attr tuned = FALSE", {
  expect_false(attr(cl, "tuned"))
  expect_true(is.model_list(cl))
})

test_that("can predict on flash models", {
  expect_s3_class(predict(cl), "predicted_df")
  expect_s3_class(predict(reg, dd[10:1, ]), "predicted_df")
})

test_that("outcome positive class is the reference level", {
  expect_equal(levels(cl$`eXtreme Gradient Boosting`$trainingData$.outcome)[1], "Y")
})

test_that("AUPR is correct", {
  pr <- flash_models(cl_prep, diabetes, models = "xgb", metric = "PR")
  carets_aupr <- pr$`eXtreme Gradient Boosting`$results$AUC[1]
  actual_aupr <- evaluate(pr)[["AUPR"]]
  expect_equal(carets_aupr, actual_aupr)
})

test_that("flash supports various loss functions in classification", {
  expect_warning(
    flash_models(d = cla_df, outcome = diabetes, model_class = "classification",
                metric = "PR", models = "xgb", n_folds = 2)
    , regexp = NA)

  # throws error when metric from other class
  expect_warning(
    flash_models(d = cla_df, outcome = diabetes, model_class = "classification",
                metric = "Rsquared", models = "xgb", n_folds = 2))
  # throws error when NA
  expect_warning(
    flash_models(d = cla_df, outcome = diabetes, model_class = "classification",
                metric = NA, models = "xgb", n_folds = 2)
  )
})

test_that("flash supports various loss functions in regression", {
  expect_warning(
    flash_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
                metric = "MAE", models = "rf", n_folds = 2)
    , regexp = NA)
  expect_warning(
    flash_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
                metric = "Rsquared", models = "rf", n_folds = 2)
    , regexp = NA)

  # throws error when metric from other class
  expect_warning(
    flash_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
                metric = "PR", models = "rf", n_folds = 2))
  # throws error when NA
  expect_warning(
    flash_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
                metric = NA, models = "rf", n_folds = 2))
})
