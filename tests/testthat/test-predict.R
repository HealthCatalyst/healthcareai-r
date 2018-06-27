### WARNING: Running this script removes all .txt files from the current directory
context("Test predict")

## Cleanup log files
remove_logfiles <- function() {
  txt_files <- list.files(pattern = "txt$", full.names = TRUE, recursive = FALSE)
  file.remove(txt_files)
}
remove_logfiles()

### Setup. Data from caret.
set.seed(2570)
data("swiss")
swiss <-
  swiss %>%
  tibble::rownames_to_column("province") %>%
  dplyr::mutate(Catholic = ifelse(Catholic > 70, "Y", "N")) %>%
  tibble::as_tibble()
part <- split_train_test(swiss, Catholic, .8)
training_data <- part$train
test_data <- part$test
test_data_newlevel <- test_data
test_data_newlevel$Catholic[sample(nrow(test_data), 3)] <- "unobserved in training"
test_data_newlevel$Catholic[5] <- "another new level"
test_data_new_missing <- test_data
test_data_new_missing$Agriculture[1:3] <- NA
test_data_new_missing$Catholic[6:7] <- NA
# Need to test combos of:
# reg/class x d from prep_data/not x newdata from prep_data/not
## Except training data not prepped and newdata prepped; that shouldn't work
model_classify_prepped <-
  training_data %>%
  prep_data(province, outcome = Catholic, make_dummies = TRUE) %>%
  tune_models(Catholic)
model_classify_not_prepped <-
  training_data %>%
  dplyr::select(-province) %>%
  tune_models(Catholic)
# Warning when RF can't find a good cut
suppressWarnings({
  model_regression_prepped <-
    training_data %>%
    prep_data(province, outcome = Fertility, make_dummies = TRUE) %>%
    tune_models(Fertility)
  model_regression_not_prepped <-
    training_data %>%
    dplyr::select(-province) %>%
    dplyr::mutate(Catholic = ifelse(Catholic == "Y", 1L, 0L)) %>%
    tune_models(Fertility)
})
# And prepped newdata to go with them
test_data_reg_prep <- prep_data(test_data, recipe = model_regression_prepped)
test_data_class_prep <- prep_data(test_data, recipe = model_classify_prepped)

# Output
regression_prepped_prepped <-
  predict(model_regression_prepped, test_data_reg_prep)
classification_prepped_prepped <-
  predict(model_classify_prepped, test_data_class_prep)

regression_not_not <-
  dplyr::mutate(test_data, Catholic = ifelse(Catholic == "Y", 1L, 0L)) %>%
  predict(model_regression_not_prepped, .)
classification_not_not <-
  predict(model_classify_not_prepped, test_data)

regression_prepped_not <-
  predict(model_regression_prepped, test_data)
suppressWarnings(classification_prepped_not <-
                   predict(model_classify_prepped, test_data))

# Test for some messages and warnings when whether to prep before predict is unclear:
# Here, should get a warning because all predictors are numeric, so they appear
# to have been prepped even when they haven't, which triggers warning.
suppressWarnings({
  pcpn_message <- capture_message( predict(model_classify_prepped, test_data) )
})
prpn_message <- capture_message( predict(model_regression_prepped, test_data) )
pcpn_warning <- capture_warning( predict(model_classify_prepped, test_data) )

test_that("When training data is prepped but test isn't, it gets prepped", {
  expect_true(grepl("Prepping", prpn_message))
  expect_true(grepl("Prepping", pcpn_message))
})
test_that("When training data hasn't been prepped but has all the same columns as prepped training, get warning", {
  expect_true(grepl("Warning", pcpn_warning))
})

test_that("predict regression returns a tibble", {
  expect_s3_class(regression_prepped_not, "tbl_df")
  expect_s3_class(regression_prepped_prepped, "tbl_df")
  expect_s3_class(regression_not_not, "tbl_df")
})

test_that("predict classification returns a tibble", {
  expect_s3_class(classification_prepped_not, "tbl_df")
  expect_s3_class(classification_prepped_prepped, "tbl_df")
  expect_s3_class(classification_not_not, "tbl_df")
})

test_that("prepping data inside or before predict produces same output", {
  expect_true(all.equal(regression_prepped_not$predicted_Fertility,
                        regression_prepped_prepped$predicted_Fertility))
  expect_true(all.equal(classification_prepped_not$predicted_Catholic,
                        classification_prepped_prepped$predicted_Catholic))
})

test_that("predictions are better than chance", {
  # Classification: predicted probs for actual Ys are greater than for actual Ns
  classification_prepped_not %>%
    dplyr::group_by(Catholic) %>%
    dplyr::summarize(mean_predicted_prob = mean(predicted_Catholic)) %>%
    with(., mean_predicted_prob[Catholic == "Y"] >
           mean_predicted_prob[Catholic == "N"]) %>%
    expect_true()
  # Regression: residuals are less than mean prediction
  with(regression_prepped_not,
       mean(abs(predicted_Fertility - Fertility)) < mean(abs(mean(Fertility) - Fertility))
  ) %>%
    expect_true()
})

test_that("If newdata isn't provided, make predictions on training data", {
  pc <- predict(model_classify_prepped)
  expect_s3_class(pc, "predicted_df")
  expect_true(all(c("Catholic", "predicted_Catholic") %in% names(pc)))
  pr <- predict(model_regression_not_prepped)
  expect_s3_class(pr, "predicted_df")
  expect_true(all(c("Fertility", "predicted_Fertility") %in% names(pr)))
})

test_that("predict can handle binary character non Y/N columns", {
  training_data %>%
    dplyr::mutate(Catholic = ifelse(Catholic == "Y", "yes", "no")) %>%
    machine_learn(province, outcome = Catholic) %>%
    predict() %>%
    expect_s3_class("data.frame")
  training_data %>%
    dplyr::mutate(Catholic = factor(ifelse(Catholic == "Y", "cath", "other"))) %>%
    machine_learn(province, outcome = Catholic) %>%
    predict() %>%
    expect_s3_class("data.frame")
})

test_that("predict handles new levels on model_list from prep_data", {
  expect_warning(preds <- predict(model_regression_prepped, test_data_newlevel))
  expect_s3_class(preds, "predicted_df")
})

test_that("predict handles missingness where unobserved in training prep_data", {
  expect_warning(preds <- predict(model_regression_prepped, test_data_new_missing))
  expect_s3_class(preds, "predicted_df")
})

test_that("predict doesn't need columns ignored in training", {
  # expect_warning just catches an expected warning. The real test here is that
  # the code doesn't error.
  expect_warning(predict(model_classify_prepped,
                         dplyr::select(test_data, -province)))
})

test_that("Warnings are issued if new factor levels are present in prediction", {
  expect_warning(predict(model_regression_prepped, test_data_newlevel),
                 "another new level")
})

test_that("Warnings are not issued for new levels in ignored columns", {
  warnings <- capture_warnings(predict(model_classify_prepped, test_data))
  expect_false(any(purrr::map_lgl(warnings, ~ grepl("province", .x))))
})

test_that("Warnings are issued if there is new missingness in predict", {
  expect_warning(predict(model_classify_prepped, test_data_new_missing),
                 "Agriculture")
  expect_warning(preds <- predict(model_regression_prepped, test_data_new_missing),
                 "Agriculture")
  expect_s3_class(preds, "predicted_df")
})

test_that("Missing values don't generate new factor level warning", {
  w <- capture_warnings(predict(model_classify_prepped, test_data_new_missing))
  expect_false(any(stringr::str_detect(w, "Catholic: NA")))
})

test_that("prepped and predicted data frame gets printed as predicted and not prepped df", {
  capture_output(mes <- capture_messages(print(regression_prepped_prepped)))
  expect_false(stringr::str_detect(mes, "prepped"))
  expect_true(stringr::str_detect(mes, "predicted"))

  capture_output(mes <- capture_messages(print(classification_prepped_prepped)))
  expect_false(stringr::str_detect(mes, "prepped"))
  expect_true(stringr::str_detect(mes, "predicted"))
})

test_that("printing predicted df prints the data frame", {
  out <- capture_output(print(regression_prepped_not))
  expect_true(stringr::str_detect(out, "tibble"))
})

test_that("printing classification df gets ROC/PR metric right", {
  suppressWarnings({
    roc <-
      training_data %>%
      prep_data(province, outcome = Catholic, make_dummies = TRUE) %>%
      tune_models(Catholic, models = "RF", metric = "ROC", tune_depth = 2, n_folds = 2) %>%
      predict()
    pr <-
      training_data %>%
      prep_data(province, outcome = Catholic, make_dummies = TRUE) %>%
      tune_models(Catholic, models = "RF", metric = "PR", tune_depth = 2, n_folds = 2) %>%
      predict()
  })
  expect_false(stringr::str_detect(as.character(capture_message(print(roc))), "PR"))
  expect_false(stringr::str_detect(as.character(capture_message(print(pr))), "ROC"))
})

test_that("determine_prep FALSE when no recipe on model", {
  determine_prep(model_regression_not_prepped, test_data) %>%
    expect_false()
})

test_that("determine_prep FALSE when newdata has been prepped", {
  determine_prep(model_regression_prepped, test_data_reg_prep) %>%
    expect_false()
})

test_that("determine_prep TRUE w/o warning when prep needed and vars changed in prep", {
  expect_warning(need_prep <- determine_prep(model_regression_prepped, test_data), NA)
  expect_true(need_prep)
})

test_that("determine_prep warns when prepped_df class stripped from newdata", {
  class(test_data_reg_prep) <- "data.frame"
  expect_warning(need_prep <- determine_prep(model_regression_prepped, test_data_reg_prep), "prep")
  expect_true(need_prep)
})

test_that("ready_no_prep preps appropriately", {
  prepped <- ready_no_prep(model_regression_not_prepped[[1]]$trainingData, test_data)
  expect_s3_class(prepped, "data.frame")
  tr_names <- setdiff(names(model_regression_not_prepped[[1]]$trainingData), ".outcome")
  expect_setequal(names(prepped), tr_names)
})

test_that("ready_no_prep stops for missingness but not in outcome", {
  test_data$Fertility[1] <- NA
  expect_s3_class(ready_no_prep(model_regression_not_prepped[[1]]$trainingData, test_data),
                  "data.frame")
  test_data$Agriculture[1] <- NA
  expect_error(ready_no_prep(model_regression_not_prepped[[1]]$trainingData, test_data),
               "missing")
})

test_that("ready_with_prep preps appropriately", {
  prepped <- ready_with_prep(model_regression_prepped, test_data)
  expect_s3_class(prepped, "data.frame")
  prepped_predictors <- setdiff(names(prepped), attr(model_regression_prepped, "target"))
  predictors <-
    model_regression_prepped$`Random Forest`$trainingData %>%
    names() %>%
    .[. != ".outcome"]
  expect_setequal(prepped_predictors, predictors)
})

test_that("ready_with_prep warns for new missingness but not in outcome", {
  test_data$Fertility[1] <- NA
  expect_s3_class(ready_with_prep(model_regression_prepped, test_data), "data.frame")
  test_data$Agriculture[1] <- NA
  expect_warning(ready_with_prep(model_regression_prepped, test_data), "missing")
})

test_that("ready_with_prep warns for new factor levels (via prep_data)", {
  test_data$Catholic[1] <- "that's weird"
  expect_warning(ready_with_prep(model_regression_prepped, test_data),
                 "Catholic: that's weird")
})

test_that("predict handles positive class specified in training", {
  d <- tibble::tibble(y = rbinom(50, 1, .5),
                      x1 = rnorm(50, mean = y, sd = 1),
                      x2 = rnorm(50, mean = y, sd = 1))
  d$x3 <- map_chr(d$y, ~ sample(c("a", "b"), 1, FALSE, if (.x) c(2, 1) else c(1, 2)))
  pd <- prep_data(d, outcome = y)
  # Default Y is positive
  preds <- list(
    tm_rf = pd %>% tune_models(y, tune_depth = 2, models = "rf") %>% predict(),
    tm_xgb = pd %>% tune_models(y, tune_depth = 2, models = "xgb") %>% predict(),
    ml = machine_learn(d, outcome = y, models = "rf", tune_depth = 2) %>% predict()
  )
  expect_true(all(map_lgl(preds, ~ {
    mean(.x$predicted_y[.x$y == "Y"]) >= mean(.x$predicted_y[.x$y == "N"])
  })))
  # Set N as positive
  preds <- list(
    tm_rf = pd %>% tune_models(y, tune_depth = 2, models = "rf", positive_class = "N") %>% predict(),
    tm_xgb = pd %>% tune_models(y, tune_depth = 2, models = "xgb", positive_class = "N") %>% predict(),
    ml = machine_learn(d, outcome = y, models = "rf", tune_depth = 2, positive_class = "N") %>% predict()
  )
  expect_true(all(map_lgl(preds, ~ {
    mean(.x$predicted_y[.x$y == "Y"]) <= mean(.x$predicted_y[.x$y == "N"])
  })))
})

test_that("can predict on untuned classification model with new data", {
  d <- na.omit(pima_diabetes)[1:100, ]
  dtest <- na.omit(pima_diabetes)[101:110, ]
  c_models <- machine_learn(d, patient_id, outcome = diabetes, tune = FALSE, n_folds = 2)
  c_preds_test <- predict(c_models, dtest)
  expect_s3_class(c_preds_test, "predicted_df")
})

test_that("predict without new data returns out of fold predictions from training", {
  preds <- predict(model_classify_prepped)$predicted_Catholic
  oofpreds <- dplyr::arrange(model_classify_prepped[[extract_model_info(model_classify_prepped)$best_model_name]]$pred, rowIndex)$Y
  expect_true(all.equal(preds, oofpreds))
})

test_that("get_oof_predictions seems to work", {
  expect_true(is.numeric(get_oof_predictions(model_regression_prepped)))
  expect_true(is.numeric(get_oof_predictions(model_classify_prepped)))
  expect_true(is.numeric(get_oof_predictions(model_regression_not_prepped)))
})

test_that("logging works as expected", {
  # Log not written by default
  preds <- predict(model_regression_prepped, test_data)
  expect_equal(length(list.files(pattern = "txt$")), 0L)
  # Activating default logs works
  predict(model_regression_prepped, test_data, write_log = TRUE)
  expect_true(file.exists("Fertility_prediction_log.txt"))
  first_log <- readLines("Fertility_prediction_log.txt")
  expect_true(any(stringr::str_detect(first_log, "Days since model trained")))
  # Appends same file
  predict(model_regression_prepped, test_data[1, ], write_log = TRUE)
  expect_equal(length(list.files(pattern = "txt$")), 1L)
  second_log <- readLines("Fertility_prediction_log.txt")
  expect_true(length(second_log) > length(first_log))
  # Custom file location
  fileloc <- tempfile(pattern = "temp-testfile-", tmpdir = ".", fileext = ".txt")
  predict(model_regression_prepped, test_data, write_log = fileloc)
  expect_true(file.exists(fileloc))
})

test_that("get_pred_summary seems to work", {
  expect_true(tibble::is.tibble(get_pred_summary(classification_prepped_prepped)))
  expect_equal(dim(get_pred_summary(classification_prepped_prepped)), c(1, 6))
})

remove_logfiles()
