context("checking untuned models")

set.seed(2057)
dd <- na.omit(pima_diabetes)[51:100, ]
cl_prep <- prep_data(dd, patient_id, outcome = diabetes)
cl <- flash_models(cl_prep, diabetes, models = "xgb")
reg <- dd %>%
  prep_data(patient_id, outcome = age) %>%
  flash_models(age, models = "glm")

# similar tests to tune_models added
td <- na.omit(pima_diabetes)[1:40, ]
reg_df <- prep_data(td, patient_id, outcome = plasma_glucose)
cla_df <- prep_data(td, patient_id, outcome = diabetes)

# Using a more complicated Multiclass dataframe
set.seed(1998)
n <- nrow(mpg)
m_df <- mpg %>%
  mutate(year = replace(year, sample(1:n, 10), NA),
         cyl = replace(cyl, sample(1:n, 20), NA),
         fl = replace(fl, sample(1:n, 10), NA),
         cty = replace(cty, sample(1:n, 30), NA))
m_df_clean <- m_df %>% prep_data(model, outcome = drv)
multi <- flash_models(d = m_df_clean, outcome = drv, models = c("xgb", "rf"))

m_df2 <-
  pima_diabetes %>%
  filter(!is.na(weight_class)) %>%
  mutate(many_chars = as.character(sample(1:100, 757, replace = TRUE))) %>%
  prep_data(outcome = many_chars)

test_that("flash_models returns appropriate model_list", {
  expect_s3_class(cl, "model_list")
  expect_s3_class(cl, "classification_list")
  expect_s3_class(reg, "model_list")
  expect_s3_class(reg, "regression_list")
  expect_s3_class(multi, "model_list")
  expect_s3_class(multi, "multiclass_list")
})

test_that("flash_models lets user select model", {
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
  expect_s3_class(predict(multi, m_df[1:7, ]), "predicted_df")
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

test_that("flash_models doesn't need an outcome specified", {
  m <- flash_models(cl_prep)
  expect_s3_class(m, "model_list")
  expect_s3_class(m, "classification_list")
})

test_that("multiclass warns when classes are sparse", {
  expect_warning(
    machine_learn(m_df2, patient_id, outcome = many_chars,
                  models = "rf", tune = FALSE),
    "sparse")
})
