context("checking untuned models")

set.seed(2057)
dd <- na.omit(pima_diabetes)[51:100, ]
cl_prep <- prep_data(dd, patient_id, outcome = diabetes)
cl <- flash_models(cl_prep, diabetes, models = "xgb")
reg <- dd %>%
  prep_data(patient_id, outcome = age) %>%
  flash_models(age, models = "glm")

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

test_that("AUPR is correct", {
  pr <- flash_models(cl_prep, diabetes, models = "xgb", metric = "PR")
  carets_aupr <- pr$`eXtreme Gradient Boosting`$results$AUC[1]
  actual_aupr <- evaluate(pr)[["AUPR"]]
  expect_equal(carets_aupr, actual_aupr)
})

test_that("flash_ and tune_ error informatively if training data isn't prepped", {
  expect_error(flash_models(mtcars, outcome = mpg), "no_prep")
  expect_error(tune_models(mtcars, outcome = am), "no_prep")
})
