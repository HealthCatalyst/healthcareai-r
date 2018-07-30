context("model_list tests setup")

# Setup ------------------------------------------------------------------------
set.seed(257056)
short <- dplyr::sample_n(na.omit(pima_diabetes), 50)
dreg <- prep_data(short, outcome = pedigree)
dcla <- prep_data(short, outcome = diabetes)
suppressWarnings({
  rf <- caret::train(x = dplyr::select(dreg, -pedigree),
                     y = dreg$pedigree,
                     method = "ranger",
                     tuneLength = 2,
                     trControl = caret::trainControl(savePredictions = "final",
                                                     method = "cv", search = "grid")
  )
  xg <- caret::train(x = dplyr::select(dreg, -pedigree),
                     y = dreg$pedigree,
                     method = "xgbTree",
                     tuneLength = 2,
                     trControl = caret::trainControl(savePredictions = "final",
                                                     method = "cv", search = "grid")
  )
  gl <- caret::train(x = dplyr::select(dreg, -pedigree),
                     y = dreg$pedigree,
                     method = "glmnet",
                     trControl = caret::trainControl(savePredictions = "final",
                                                     method = "cv", search = "grid")
  )
})
# Implicit test that warning not issued for missing resampled performance metrics:
r_models <- tune_models(dreg, pedigree, tune_depth = 2, n_folds = 2, models = c("rf", "glm"))
c_models <- tune_models(dcla, diabetes, tune_depth = 2, n_folds = 2, model_name = "great_name", models = "xgb")
c_pr <- flash_models(dcla, diabetes, metric = "PR", n_folds = 2, models = "xgb")
single_model_as <- as.model_list(rf)
single_model_tune <- tune_models(dcla, diabetes, models = "xgb", n_folds = 2, tune_depth = 2)
double_model_as <- as.model_list(rf, xg)
r_flash <- flash_models(dreg, pedigree, n_folds = 2, models = "xgb")
c_flash <- flash_models(dcla, diabetes, n_folds = 2, models = "xgb")
unprepped_flash <- flash_models(dplyr::select(short, pregnancies, age, pedigree),
                                pedigree, models = "rf", n_folds = 2)
ods <- list(
  prepped = attr(c_models, "original_data_str"),
  unprepped = attr(unprepped_flash, "original_data_str"),
  as = attr(single_model_as, "original_data_str")
)

context("Checking model_list constructors") # ----------------------------------

test_that("as.model_list works same with different argument specs", {
  expect_equivalent(as.model_list(rf),
                    as.model_list(listed_models = list(rf)))
  expect_equivalent(as.model_list(rf),
                    as.model_list(rf, model_class = "regression"))
})

test_that("as.model_list fails if model_class is unsupported", {
  expect_error(as.model_list(model_class = "what diabetes i even?"))
})

test_that("as.model_list errors if input isn't a caret model", {
  expect_error(as.model_list(1:5, model_class = "regression"))
  expect_error(as.model_list("HEY"))
  expect_error(as.model_list(ranger::ranger(pedigree ~ ., dreg)))
})

test_that("as.model_list succeeds with one or more models as input", {
  expect_s3_class(as.model_list(rf, model_class = "regression"),
                  "model_list")
  expect_s3_class(as.model_list(rf, xg), "model_list")
  expect_s3_class(
    as.model_list(listed_models = list(rf, xg), model_class = "regression"),
    "model_list"
  )
  expect_s3_class(as.model_list(listed_models = list(rf)), "model_list")
})

test_that("as.model_list returns correct model names (from modelInfo$label)", {
  correct_names <- names(r_models)
  m_list <- structure(list(rf, gl), names = c("rando", "lasso"))
  expect_equal(
    names(as.model_list(listed_models = m_list)),
    correct_names
  )
  expect_equal(
    names(as.model_list(rf, gl)),
    correct_names
  )
})

test_that("as.model_list tuned-argument works", {
  expect_true(attr(as.model_list(rf), "tuned"))
  expect_false(attr(as.model_list(rf, tuned = FALSE), "tuned"))
})

test_that("model_lists have original data str as zero-row DF with right names and classes", {
  purrr::map_lgl(ods, is.data.frame) %>% all() %>% expect_true()
  purrr::map_lgl(ods, ~ nrow(.x) == 0) %>% all() %>% expect_true()
  expect_equivalent(ods$prepped, short[0, -which(names(short) == "diabetes")])
  expect_setequal(names(ods$unprepped), c("pregnancies", "age"))
  expect_equivalent(ods$as, dplyr::select(dreg[0, ], -pedigree))
})

test_that("model_list's original_data_str is the same as predict's return", {
  preds <- purrr::map2(
    .x = list(c_models, unprepped_flash),
    .y = list(short, dplyr::select(short, pregnancies, age, pedigree)),
    .f = ~ suppressWarnings( predict(.x, .y)[0, - (1:2)] )
  )
  purrr::map2_lgl(preds, ods[1:2], ~ isTRUE(all.equal(.x, .y))) %>%
    all() %>%
    expect_true()
})

test_that("model_lists have r, hcai, and other-package versions as attrs", {
  expected <- c("r_version", "hcai_version", "other_packages")
  expect_true(all(expected %in% names(attributes(r_models)$versions)))
  expect_true(all(expected %in% names(attributes(single_model_as)$versions)))
  expect_true(all(expected %in% names(attributes(unprepped_flash)$versions)))
})

context("Checking model_list generics") # --------------------------------------

test_that("plot.model_list works on regression_list", {
  expect_equal(class(plot(r_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot.model_list(r_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_error(plot.model_list(ranger::ranger(pedigree ~ ., dreg), print = FALSE),
               regexp = "model_list")
  r2 <- tune_models(dreg, pedigree, models = "rf", metric = "Rsquared")
  expect_s3_class(plot(r2, print = FALSE), "gg")
})

test_that("plot.model_list works on classification_list", {
  expect_equal(class(plot(c_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot.model_list(c_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot(c_pr, print = FALSE)),
               c("gg", "ggplot"))
  expect_error(plot.model_list(ranger::ranger(diabetes ~ ., dcla), print = FALSE),
               regexp = "model_list")

  # With PR as the metric
  expect_equal(class(plot(c_pr, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot.model_list(c_pr, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot(c_pr, print = FALSE)),
               c("gg", "ggplot"))
})

test_that("print.model_list works", {
  rprint <- capture_output(r_models, TRUE)
  expect_true(nchar(rprint) > 0)
  expect_true(grepl("regression", rprint, ignore.case = TRUE))

  cprint <- capture_output(c_models, TRUE)
  expect_true(nchar(cprint) > 0)
  expect_true(grepl("classification", cprint, ignore.case = TRUE))

  # With PR as the metric
  cprint <- capture_output(c_pr, TRUE)
  expect_true(nchar(cprint) > 0)
  expect_true(grepl("PR", cprint, ignore.case = TRUE))

  # Model name
  rprint <- capture_output(r_models, TRUE)
  expect_true(grepl("Model Name: pedigree", rprint, ignore.case = TRUE))
  cprint <- capture_output(c_models, TRUE)
  expect_true(grepl("Model Name: great_name", cprint, ignore.case = TRUE))
})

test_that("summary.model_list works", {
  rsumout <- capture_output(rsum <- summary(r_models), TRUE)
  expect_true(nchar(rsumout) > 0)
  expect_true(grepl("hyperparameters", rsumout, ignore.case = TRUE))
  expect_true(is.list(rsum))
  expect_true(rlang::is_named(rsum))

  csumout <- capture_output(csum <- summary(c_models), TRUE)
  expect_true(nchar(csumout) > 0)
  expect_true(grepl("hyperparameters", csumout, ignore.case = TRUE))
  expect_true(is.list(csum))
  expect_true(rlang::is_named(csum))

  # With PR as the metric
  csumout <- capture_output(csum <- summary(c_pr), TRUE)
  expect_true(nchar(csumout) > 0)
  expect_true(grepl("Precision", csumout, ignore.case = TRUE))
  expect_true(is.list(csum))
  expect_true(rlang::is_named(csum))
})

context("Checking model_list generics on untuned model_lists") #----------------

test_that("print.model_list works with untuned_model_lists", {
  expect_warning(flash_r_print <- capture_output(print(r_flash)), NA)
  expect_warning(flash_c_print <- capture_output(print(c_flash)), NA)
  expect_false(grepl("Inf", flash_r_print))
  expect_false(grepl("Inf", flash_c_print))
  expect_true(grepl("Target: pedigree", flash_r_print))
  expect_true(grepl("Target: diabetes", flash_c_print))
  expect_true(grepl("Models have not been tuned", flash_r_print))
  expect_true(grepl("selected hyperparameter values", flash_c_print))
})

test_that("summary.model_list works with untuned_model_lists", {
  expect_warning(flash_r_summary <- capture_output(summary(r_flash)), NA)
  expect_warning(flash_c_summary <- capture_output(summary(c_flash)), NA)
  expect_false(grepl("Inf", flash_r_summary))
  expect_false(grepl("Inf", flash_c_summary))
  expect_false(grepl("0 rows", flash_r_summary))
  expect_false(grepl("Best performance:", flash_r_summary))
  expect_true(grepl("Best algorithm:", flash_c_summary))
})

test_that("plot.model_list works with message untuned_model_lists", {
  expect_warning(flash_r_plot <- plot(r_flash, print = FALSE), NA)
  expect_warning(flash_c_plot <- plot(c_flash, print = FALSE), NA)
  expect_message(plot(c_flash, print = FALSE))
  expect_s3_class(flash_r_plot, "gg")
  expect_s3_class(flash_c_plot, "gg")
})

context("Testing model list utilities") # --------------------------------------
test_that("change_metric_names changes AUC to AUPR and prints", {
  m <- change_metric_names(c_pr)
  expect_true(all(c("AUPR", "Precision", "Recall") %in% names(m[[1]]$results)))
  expect_true(all(c("AUPR", "Precision", "Recall") %in% names(m[[1]]$results)))
  expect_error(capture_output(print(m)), NA)
})

test_that("change_metric_names changes ROC to AUROC", {
  m <- change_metric_names(c_models)
  expect_true(all(c("AUROC", "Sens", "Spec") %in% names(m[[1]]$results)))
  expect_true(all(c("AUROC", "Sens", "Spec") %in% names(m[[1]]$results)))
  expect_error(capture_output(print(m)), NA)
})

test_that("Change PR metric doesn't change object class", {
  expect_setequal(class(change_metric_names(c_pr)), class(c_pr))
  expect_setequal(class(change_metric_names(c_models)), class(c_models))
  preds <- predict(c_models)
  expect_setequal(class(change_metric_names(preds)), class(preds))
})

test_that("model_lists have time model trained attribute", {
  check_timestamp <- function(m) expect_true(lubridate::is.POSIXt(attr(m, "timestamp")))
  check_timestamp(r_models)
  check_timestamp(c_models)
  check_timestamp(c_pr)
  check_timestamp(r_flash)
  check_timestamp(c_flash)
})

test_that("model_lists only carry one copy of training data", {
  expect_s3_class(r_models[[1]]$trainingData, "data.frame")
  expect_s3_class(c_models[[1]]$trainingData, "data.frame")
  expect_s3_class(double_model_as[[1]]$trainingData, "data.frame")
  expect_s3_class(single_model_as[[1]]$trainingData, "data.frame")
  expect_s3_class(single_model_tune[[1]]$trainingData, "data.frame")

  expect_null(r_models[[2]]$trainingData)
  expect_null(double_model_as[[2]]$trainingData)
})

test_that("[ extracts models by index", {
  expect_s3_class(r_flash[1], "model_list")
  expect_s3_class(r_models[2], "model_list")
  expect_s3_class(r_models[1:2], "model_list")
  expect_equivalent(r_models[seq_along(r_models)], r_models)
})

test_that("[ extracts by name, index, or logical vector", {
  expect_equivalent(double_model_as[1], double_model_as[names(double_model_as)[1]])
  expect_equivalent(c_models[1], c_models[names(c_models)[1]])
  expect_equivalent(double_model_as[1], double_model_as[c(TRUE, FALSE)])
  expect_equivalent(double_model_as[2], double_model_as[names(double_model_as)[2]])
  expect_equivalent(double_model_as[1:2], double_model_as[names(double_model_as)[1:2]])
  expect_equivalent(double_model_as, double_model_as[c(TRUE, TRUE)])
})

test_that("metrics and predict are same for extracted best model", {
  best <- extract_model_info(c_models)$best_model_name
  extracted <- c_models[best]
  expect_equivalent(evaluate(c_models), evaluate(extracted))
  expect_equivalent(predict(c_models), predict(extracted))
})

test_that("performance drops if the best model is pulled out of model_list", {
  mi <- extract_model_info(r_models)
  not_best <- which(!names(r_models) %in% mi$best_model_name)
  original_rmse <- attr(r_models, "performance")[mi$metric]
  subsetted_rmse <- attr(r_models[not_best], "performance")[mi$metric]
  expect_true(original_rmse < subsetted_rmse)
})

test_that("extraction from model list doesn't change model-trained timestamp", {
  expect_equal(attr(c_models, "timestamp"), attr(c_models[1], "timestamp"))
})
