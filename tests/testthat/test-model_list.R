context("model_list tests setup")

# Setup ------------------------------------------------------------------------
data(mtcars)
mtcars$am <- as.factor(c("automatic", "manual")[mtcars$am + 1])
set.seed(257056)
rf <- caret::train(x = dplyr::select(mtcars, -mpg),
                   y = mtcars$mpg,
                   method = "ranger",
                   tuneLength = 2,
                   trControl = caret::trainControl(savePredictions = "final")
)
kn <- caret::train(x = dplyr::select(mtcars, -mpg),
                   y = mtcars$mpg,
                   method = "kknn",
                   tuneLength = 2,
                   trControl = caret::trainControl(savePredictions = "final")
)
r_models <- tune_models(mtcars, mpg, n_folds = 2, tune_depth = 2)
c_models <- tune_models(mtcars, am, n_folds = 2, tune_depth = 2)
c_pr <- tune_models(mtcars, am, metric = "PR", n_folds = 2, tune_depth = 2)
single_model_as <- as.model_list(rf)
single_model_tune <- tune_models(mtcars, am, models = "rf")
double_model_as <- as.model_list(rf, kn)
r_flash <- flash_models(mtcars, mpg)
c_flash <- flash_models(mtcars, am)

context("Checking model_list constructors") # ----------------------------------

test_that("as.model_list works same with different argument specs", {
  expect_equivalent(as.model_list(rf),
                    as.model_list(listed_models = list(rf)))
  expect_equivalent(as.model_list(rf),
                    as.model_list(rf, model_class = "regression"))
})

test_that("as.model_list fails if model_class is unsupported", {
  expect_error(as.model_list(model_class = "what am i even?"))
})

test_that("as.model_list errors if input isn't a caret model", {
  expect_error(as.model_list(1:5, model_class = "regression"))
  expect_error(as.model_list("HEY"))
  expect_error(as.model_list(ranger::ranger(mpg ~ ., mtcars)))
})

test_that("as.model_list succeeds with one or more models as input", {
  expect_s3_class(as.model_list(rf, model_class = "regression"),
                  "model_list")
  expect_s3_class(as.model_list(rf, kn), "model_list")
  expect_s3_class(
    as.model_list(listed_models = list(rf, kn), model_class = "regression"),
    "model_list"
  )
  expect_s3_class(as.model_list(listed_models = list(rf)), "model_list")
})

test_that("as.model_list returns correct model names (from modelInfo$label)", {
  correct_names <- names(r_models)
  m_list <- structure(list(rf, kn), names = c("rando", "knn"))
  expect_equal(
    names(as.model_list(listed_models = m_list)),
    correct_names
  )
  expect_equal(
    names(as.model_list(rf, kn)),
    correct_names
  )
})

test_that("as.model_list tuned-argument works", {
  expect_true(attr(as.model_list(rf), "tuned"))
  expect_false(attr(as.model_list(rf, tuned = FALSE), "tuned"))
})

context("Checking model_list generics") # --------------------------------------

test_that("plot.model_list works on regression_list", {
  expect_equal(class(plot(r_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot.model_list(r_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_error(plot.model_list(ranger::ranger(mpg ~ ., mtcars), print = FALSE),
               regexp = "model_list")
  r2 <- tune_models(mtcars, mpg, models = "rf", metric = "Rsquared")
  expect_s3_class(plot(r2, print = FALSE), "gg")
})

test_that("plot.model_list works on classification_list", {
  expect_equal(class(plot(c_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot.model_list(c_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot(c_pr, print = FALSE)),
               c("gg", "ggplot"))
  expect_error(plot.model_list(ranger::ranger(am ~ ., mtcars), print = FALSE),
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
  expect_true(grepl("Target: mpg", flash_r_print))
  expect_true(grepl("Target: am", flash_c_print))
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
  expect_message(plot(c_flash, print = FALSE), "not much to plot")
  expect_s3_class(flash_r_plot, "gg")
  expect_s3_class(flash_c_plot, "gg")
})

context("Testing model list utilities") # --------------------------------------
test_that("change_metric_names changes AUC to AUPR", {
  m <- change_metric_names(c_pr)

  expect_true(
    all(c("AUPR", "Precision", "Recall") %in% names(
      m$`Random Forest`$results)))

  expect_true(
    all(c("AUPR", "Precision", "Recall") %in% names(
      m$`k-Nearest Neighbors`$results)))
})

test_that("change_metric_names changes ROC to AUROC", {
  m <- change_metric_names(c_models)

  expect_true(
    all(c("AUROC", "Sens", "Spec") %in% names(
      m$`Random Forest`$results)))

  expect_true(
    all(c("AUROC", "Sens", "Spec") %in% names(
      m$`k-Nearest Neighbors`$results)))
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
  expect_null(c_models[[2]]$trainingData)
  expect_null(double_model_as[[2]]$trainingData)
})

test_that("[ extracts 1 or 2 models by index", {
  expect_s3_class(r_flash[1], "model_list")
  expect_s3_class(r_flash[2], "model_list")
  expect_equal(r_flash[1:2], r_flash)
})

test_that("[ extracts by name, index, or logical vector", {
  expect_equal(double_model_as[1], double_model_as["Random Forest"])
  expect_equal(c_models[1], c_models["Random Forest"])
  expect_equal(double_model_as[1], double_model_as[c(TRUE, FALSE)])
  expect_equal(double_model_as[2], double_model_as["k-Nearest Neighbors"])
  expect_equal(double_model_as[1:2], double_model_as[c("Random Forest", "k-Nearest Neighbors")])
  expect_equal(double_model_as, double_model_as[c(TRUE, TRUE)])
})

test_that("metrics and predict are same for extracted best model", {
  best <- extract_model_info(c_models)$best_model_name
  extracted <- c_models[best]
  expect_equal(evaluate(c_models), evaluate(extracted))
  expect_equal(predict(c_models), predict(extracted))
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
