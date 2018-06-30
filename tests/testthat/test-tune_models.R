context("test-tune_models")

# Setup ------------------------------------------------------------------------
reg_df <- na.omit(pima_diabetes)[1:100, ] %>% prep_data(patient_id, outcome = plasma_glucose)
cla_df <- na.omit(pima_diabetes)[1:100, ] %>% prep_data(patient_id, outcome = diabetes)
tm <- tune_models(cla_df, diabetes)

test_that("Error informatively if outcome class doesn't match model_class", {
  expect_error(tune_models(cla_df, diabetes, model_class = "regression"), "categorical")
  cla_df$diabetes <- factor(cla_df$diabetes)
  expect_error(tune_models(cla_df, diabetes, model_class = "regression"), "categorical")
  expect_error(tune_models(reg_df, plasma_glucose, model_class = "classification"), "numeric")
})

# No error for each algorithm x response-class type
test_that("tune doesn't error on xgb regression", {
  expect_error(
    tune_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
         models = "xgb", n_folds = 2, tune_depth = 2)
    , regexp = NA)

})

test_that("tune doesn't error on xgb classification", {
  expect_error(
    tune_models(d = cla_df, outcome = diabetes, model_class = "classification",
         models = "xgb", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune doesn't error on rf regression", {
  expect_error(
      tune_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
           models = "rf", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune doesn't error on rf classification", {
  expect_error(
    tune_models(d = cla_df, outcome = diabetes, model_class = "classification",
         models = "rf", n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune errors sensibly if outcome isn't present", {
  expect_error(tune_models(cla_df, xxx), regexp = "xxx")
})

# Training multiple models in one call
test_that("tune doesn't error on rf & xgb classification", {
  expect_error(
    tune_models(d = cla_df, outcome = diabetes, model_class = "classification",
         models = c("rf", "xgb"), n_folds = 2, tune_depth = 2)
    , regexp = NA)
})

test_that("tune returns a model_list of appropriate type", {
  c_models <-
    tune_models(d = cla_df, outcome = diabetes, model_class = "classification",
         n_folds = 2, tune_depth = 2)
    r_models <-
      tune_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
           n_folds = 2, tune_depth = 2)
  expect_s3_class(c_models, "model_list")
  expect_s3_class(c_models, "classification_list")
  expect_s3_class(r_models, "model_list")
  expect_s3_class(r_models, "regression_list")
})

test_that("tune returns a model_list of appropriate type when not specified", {
  c_models <-
    tune_models(d = cla_df, outcome = diabetes, n_folds = 2, tune_depth = 2)
    r_models <-
      tune_models(d = reg_df, outcome = plasma_glucose, n_folds = 2, tune_depth = 2)
  expect_s3_class(c_models, "model_list")
  expect_s3_class(c_models, "classification_list")
  expect_s3_class(r_models, "model_list")
  expect_s3_class(r_models, "regression_list")
})

test_that("tune errors informatively if outcome is list", {
  reg_df$plasma_glucose <- as.list(reg_df$plasma_glucose)
  expect_error(
    tune_models(d = reg_df, outcome = plasma_glucose, n_folds = 2, tune_depth = 2),
    regexp = "list")
})

# Informative erroring
test_that("tune errors informatively if the algorithm isn't supported", {
  expect_error(tune_models(reg_df, plasma_glucose, "regression", "not a model"),
               regexp = "supported")
})

test_that("Character and factor input variables produce informative error", {
  reg_df$pregnancies <- letters[reg_df$pregnancies + 1]
  expect_error(tune_models(reg_df, outcome = plasma_glucose), "pregnancies")
  reg_df$pregnancies <- factor(reg_df$pregnancies)
  expect_error(tune_models(reg_df, outcome = plasma_glucose), "pregnancies")
})

# Can handle various metrics. expect_warning because metric not found->default
test_that("tune supports various loss functions in classification", {
  expect_warning(
    tune_models(d = cla_df, outcome = diabetes, model_class = "classification",
         metric = "ROC", models = "xgb", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  # Not yet supported
  # expect_warning(
  #   tune_models(d = cla_df, outcome = diabetes, model_class = "classification",
  #               metric = "mnLogLoss", models = "xgb", n_folds = 2,
  #               tune_depth = 2)
  #   , regexp = NA)
  expect_warning(
    tune_models(d = cla_df, outcome = diabetes, model_class = "classification",
                metric = "PR", models = "xgb", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  # expect_warning(
  #   tune_models(d = cla_df, outcome = diabetes, model_class = "classification",
  #               metric = "accuracy", models = "xgb", n_folds = 2,
  #               tune_depth = 2)
  #   , regexp = NA)
})

test_that("tune supports various loss functions in regression", {
  expect_warning(
    tune_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
         metric = "MAE", models = "rf", n_folds = 2, tune_depth = 2)
    , regexp = NA)
  expect_warning(
    tune_models(d = reg_df, outcome = plasma_glucose, model_class = "regression",
         metric = "Rsquared", models = "rf", n_folds = 2,
         tune_depth = 2)
    , regexp = NA)
})

test_that("tune handles character outcome", {
  cla_df$diabetes <- as.character(cla_df$diabetes)
  expect_s3_class(tune_models(cla_df, diabetes, tune_depth = 2,
                              n_folds = 2, models = "rf"),
                  "classification_list")
})

test_that("tune handles tibble input", {
  expect_s3_class(tune_models(tibble::as_tibble(cla_df), diabetes,
                              tune_depth = 2, n_folds = 2, models = "xgb"),
                  "classification_list")
})

test_that("If a column was ignored in prep_data it's ignored in tune", {
  pd <- prep_data(reg_df, plasma_glucose, outcome = age)
  capture_warnings(mods <- tune_models(pd, age, tune_depth = 2, n_folds = 2, models = "xgb"))
  expect_false("plasma_glucose" %in% names(mods[[1]]$trainingData))
})

test_that("Missing outcome variable error points user to what's missing", {
  expect_error(tune_models(cla_df), "outcome")
})

test_that("Get informative error from setup_training if you forgot to name outcome arg in prep_data", {
  pd <- prep_data(pima_diabetes, patient_id, diabetes)
  expect_error(tune_models(pd, diabetes), "outcome")
})

test_that("tune_models attaches positive class to model_list", {
  expect_true("positive_class" %in% names(attributes(tm)))
})

test_that("By default Y is chosen as positive class for N/Y character outcome", {
  expect_equal("Y", attr(tm, "positive_class"))
})

test_that("tune_models picks Y and yes as positive class even if N/no is first", {
  cla_df$diabetes <- factor(cla_df$diabetes, levels = c("N", "Y"))
  m <- tune_models(cla_df, diabetes, "glm")
  expect_equal("Y", attr(m, "positive_class"))
  p <- predict(m)
  expect_true(mean(p$predicted_diabetes[p$diabetes == "Y"]) > mean(p$predicted_diabetes[p$diabetes == "N"]))

  cla_df$diabetes <- factor(ifelse(cla_df$diabetes == "Y", "yes", "no"),
                             levels = c("no", "yes"))
  m <- tune_models(cla_df, diabetes, "glm")
  expect_equal("yes", attr(m, "positive_class"))
  p <- predict(m)
  expect_true(mean(p$predicted_diabetes[p$diabetes == "yes"]) > mean(p$predicted_diabetes[p$diabetes == "no"]))
})

test_that("tune_models and predict respect positive class declaration", {
  cla_df$diabetes <- factor(cla_df$diabetes, levels = c("Y", "N"))
  n_ref <- tune_models(cla_df, diabetes, "rf", positive_class = "N")
  expect_equal(attr(n_ref, "positive_class"), "N")
  p <- predict(n_ref)
  expect_true(mean(p$predicted_diabetes[p$diabetes == "N"]) > mean(p$predicted_diabetes[p$diabetes == "Y"]))
})

test_that("set_outcome_class errors informatively if value not in vector", {
  expect_error(set_outcome_class(factor(letters[1:2]), "nope"), "a and b")
})

test_that("set_outcome_class sets levels as expected", {
  yn <- factor(c("Y", "N"))
  expect_equal(levels(set_outcome_class(yn, NULL))[2], "Y")
  expect_equal(levels(set_outcome_class(yn, "N"))[2], "N")
  yesno <- factor(c("yes", "no"))
  expect_equal(levels(set_outcome_class(yesno, NULL))[2], "yes")
  expect_equal(levels(set_outcome_class(yesno, "no"))[2], "no")
  other <- factor(c("admit", "nonadmit"))
  expect_equal(levels(set_outcome_class(other, NULL))[2], "admit")
  expect_equal(levels(set_outcome_class(other, "nonadmit"))[2], "nonadmit")
})

test_that("tune models takes hyperparameter grid and tunes on it", {
  rf_hyperparameters <-
    expand.grid(
      mtry = 1:5,
      splitrule = c("gini", "extratrees"),
      min.node.size = 1
    )
  grid_search_models <-
    tune_models(d = cla_df,
                outcome = diabetes,
                models = "rf",
                hyperparameters = list(rf = rf_hyperparameters)
    )
  grid_tune <- grid_search_models$`Random Forest`$results

  expect_setequal(1:5, grid_tune$mtry)
  expect_setequal(c("gini", "extratrees"), as.character(grid_tune$splitrule))
  expect_setequal(1, grid_tune$min.node.size)  # nolint
})

test_that("tune models takes a list of one-row data frames of hyperparameters and tunes on it", {
  hp <-
    list(
      xgb = data.frame(
        eta = .5,
        gamma = 0,
        max_depth = 0,
        subsample = .5,
        colsample_bytree = .5,
        min_child_weight = 2,
        nrounds = 5
      ),
      rf = data.frame(
        mtry = 3,
        splitrule = "extratrees",
        min.node.size = 3),
      glm = data.frame(
        alpha = 1,
        lambda = .001)
    )
  m <- tune_models(cla_df, diabetes, hyperparameters = hp)
  expect_s3_class(m, "classification_list")
  expect_true(all(purrr::map_int(m, ~ nrow(.x$results) == 1)))
  expect_true(m$`Random Forest`$results$mtry == 3)
})

test_that("If only tuning one model, can provide hyperparameter grid outside list", {
  hp <- expand.grid(mtry = c(3, 9), splitrule = "extratrees", min.node.size = c(1, 5))
  expect_error(tune_models(cla_df, diabetes, hyperparameters = hp), regexp = "data frame")
  m <- tune_models(cla_df, diabetes, models = "rf", hyperparameters = hp)
  expect_s3_class(m, "classification_list")
  expect_true(nrow(m$`Random Forest`$results) == 4)
})

test_that("tune_ and flash_ issues informative errors if missingness in predictor", {
  # First 50 row indices with missingness in any predictor:
  i <- which(!complete.cases(pima_diabetes[, -which(names(pima_diabetes) == "diabetes")]))[1:50]
  with_miss <- pima_diabetes[i, -1]
  expect_error(tune_models(dplyr::select(with_miss, -weight_class), diabetes), "impute")
  expect_error(flash_models(dplyr::select(with_miss, -weight_class, -diabetes), age), "impute")
  expect_error(machine_learn(with_miss, outcome = diabetes), NA)
})

test_that("outcome can be provided quoted", {
  expect_s3_class(m <- tune_models(cla_df, "diabetes"), "model_list")
  expect_s3_class(predict(m), "predicted_df")
})

test_that("tune_models, flash_models, and machine_learn issue PHI cautions", {
  phi_present <- function(messages)
    any(purrr::map_lgl(messages, stringr::str_detect, "PHI"))

  tune_messages <- capture_messages(tune_models(cla_df, diabetes))
  flash_messages <- capture_messages(flash_models(cla_df, diabetes))
  ml_messages <- capture_messages(machine_learn(cla_df, outcome = diabetes))

  expect_true(phi_present(tune_messages))
  expect_true(phi_present(flash_messages))
  expect_true(phi_present(ml_messages))
})

test_that("tune_ and flash_ models add all-unique char/factor columns to ignored", {
  # Warnings by design here and are tested in test-find_unique_columns
  suppressWarnings({
    cla_df$patient_id <- paste0("A", cla_df$patient_id)
    tm <- tune_models(cla_df, diabetes)

    expect_false("patient_id" %in% names(tm$`Random Forest`$trainingData))
    cla_df$ignore2 <- paste0("a", seq_len(nrow(cla_df)))
    tm2 <- tune_models(reg_df, plasma_glucose, models = "rf")
    expect_false(any(c("patient_id", "ignore2") %in% names(tm2$`Random Forest`$trainingData)))
    fm <- tune_models(cla_df, diabetes, models = "xgb")
    expect_false(any(c("patient_id", "ignore2") %in% names(fm[[1]]$trainingData)))

    cla_df$patient_id <- factor(cla_df$patient_id)
    tm <- tune_models(cla_df, diabetes, models = "rf")
    expect_false("patient_id" %in% names(tm$`Random Forest`$trainingData))
  })
})

test_that("Get informative error if there's not an outcome instance for each CV fold", {
  small_df <- cla_df[1:10, ]
  table(small_df$diabetes)
  expect_error(flash_models(small_df, diabetes), "cross validation fold")
  expect_error(tune_models(small_df, diabetes), "cross validation fold")
  expect_error(flash_models(small_df, diabetes, n_folds = 3, models = "rf"), NA)
})

test_that("get_original_data works", {
  d <- structure(data.frame(x = 1, y = 2),
                 original_data_str = data.frame(x = integer()))
  from_attr <- get_original_data(d, "y")
  expect_equal(from_attr, data.frame(x = integer()))
  expect_equal(from_attr,
               get_original_data(structure(d, original_data_str = NULL), "y"))
})

test_that("check_training_time works", {
  small_data <- check_training_time(ddim = c(100, 10), hpdim = c(glm = 1), n_folds = 5)
  expect_false(stringr::str_detect(small_data, "Model training"))
  expect_true(stringr::str_detect(small_data, " 9 features "))
  big_data <- check_training_time(ddim = c(1e6, 1001), hpdim = purrr::map_int(get_random_hyperparameters(), nrow),
                                  n_folds = 5)
  expect_true(stringr::str_detect(big_data, "MODEL TRAINING"))
  expect_true(stringr::str_detect(big_data, "1,000 features"))
})
