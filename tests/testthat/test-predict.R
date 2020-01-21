### WARNING: Running this script removes all .txt files from the current directory
context("Test predict")

## Cleanup log files
remove_logfiles <- function() {
  txt_files <- list.files(pattern = "txt$", full.names = TRUE, recursive = FALSE)
  file.remove(txt_files)
}
remove_logfiles()

### Setup. Data from caret.
set.seed(2572)
data("swiss")
swiss <-
  swiss %>%
  tibble::rownames_to_column("province") %>%
  dplyr::mutate(Catholic = ifelse(Catholic > 70, "Y", "N")) %>%
  tibble::as_tibble()
part <- split_train_test(swiss, Catholic, .6)
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
  prep_data(province, outcome = Catholic) %>%
  flash_models(Catholic)
model_classify_not_prepped <-
  training_data %>%
  prep_data(province, outcome = Catholic, no_prep = TRUE) %>%
  flash_models(Catholic)

model_regression_prepped <-
  training_data %>%
  prep_data(province, outcome = Fertility) %>%
  flash_models(Fertility)
model_regression_not_prepped <-
  training_data %>%
  dplyr::mutate(Catholic = ifelse(Catholic == "Y", 1L, 0L)) %>%
  prep_data(province, outcome = Fertility, no_prep = TRUE) %>%
  flash_models(Fertility)
# And prepped newdata to go with them
test_data_reg_prep <- prep_data(test_data, recipe = model_regression_prepped)
test_data_class_prep <- prep_data(test_data, recipe = model_classify_prepped)

set.seed(1234)
iris_char <- iris %>%
  mutate(potted = sample(c("Feb", "Mar", "Apr"), 150, replace = TRUE))
m_train <- iris_char
m_test <- iris_char[seq(1, 150, 10), ]
model_multi_prepped <-
  prep_data(m_train, outcome = Species, make_dummies = TRUE) %>%
  flash_models(outcome = Species)
model_multi_not_prepped <- m_train %>%
  prep_data(potted, outcome = Species, no_prep = TRUE) %>%
  flash_models(outcome = Species)

# Output
regression_prepped_prepped <- predict(model_regression_prepped, test_data)
classification_prepped_prepped <- predict(model_classify_prepped, test_data)
multi_prepped_prepped <- predict(model_multi_prepped, m_test)

regression_not_not <-
  dplyr::mutate(test_data, Catholic = ifelse(Catholic == "Y", 1L, 0L)) %>%
  predict(model_regression_not_prepped, .)
classification_not_not <- predict(model_classify_not_prepped, test_data)
classification_not_not <-
  predict(model_classify_not_prepped, test_data)
classification_not_not <-
  predict(model_classify_not_prepped, test_data)
multi_not_not <-
  predict(model_multi_not_prepped, m_test)

regression_prepped_not <- predict(model_regression_prepped, test_data)
classification_prepped_not <- predict(model_classify_prepped, test_data)
classification_prepped_not <- predict(model_classify_prepped, test_data)
multi_prepped_not <- predict(model_multi_prepped, m_test)

test_that("predict errors informatively if prepped data is passed", {
  expect_error(predict(model_regression_prepped,
                       newdata = prep_data(test_data, recipe = model_regression_prepped)),
               "prep_data")
  expect_error(predict(model_classify_prepped,
                       newdata = prep_data(test_data, recipe = model_classify_prepped)),
               "prep_data")
})

prpn_message <- capture_message(predict(model_regression_prepped, test_data))
pcpn_warning <- capture_warning(predict(model_classify_prepped, test_data))
pmpn_message <- capture_message(predict(model_multi_prepped, m_test))

test_that("When training data is prepped but test isn't, it gets prepped", {
  prpn_message <- capture_message(predict(model_regression_prepped, test_data))
  expect_true(grepl("Prepping", prpn_message))
  expect_true(grepl("Prepping", pmpn_message))
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

test_that("predict multiclass returns a tibble", {
  expect_s3_class(multi_prepped_not, "tbl_df")
  expect_s3_class(multi_prepped_prepped, "tbl_df")
  expect_s3_class(multi_not_not, "tbl_df")
})

test_that("prepping data inside or before predict produces same output", {
  expect_true(all.equal(regression_prepped_not$predicted_Fertility,
                        regression_prepped_prepped$predicted_Fertility))
  expect_true(all.equal(classification_prepped_not$predicted_Catholic,
                        classification_prepped_prepped$predicted_Catholic))
  expect_true(all.equal(multi_prepped_not$predicted_Species,
                        multi_prepped_prepped$predicted_Species))
})

test_that("predictions are better than chance", {
  # Classification: predicted probs for actual Ys are greater than for actual Ns
  classification_prepped_not %>%
    dplyr::group_by(Catholic) %>%
    dplyr::summarize(mean_predicted_prob = mean(predicted_Catholic)) %>%
    with(., mean_predicted_prob[Catholic == "Y"] >
           mean_predicted_prob[Catholic == "N"]) %>%
    expect_true()
  # Regression: numeric, not na.
  expect_true(all(regression_prepped_not$predicted_Fertility >= 0))
  expect_true(!any(is.na(regression_prepped_not$predicted_Fertility)))
  # Multi
  multi_prepped_not %>%
    dplyr::mutate(correct = Species == predicted_Species) %>%
    with(., sum(correct) >
           nrow(.) / 3) %>%
    expect_true()
})

test_that("If newdata isn't provided, make predictions on training data", {
  pc <- predict(model_classify_prepped)
  expect_s3_class(pc, "predicted_df")
  expect_true(all(c("Catholic", "predicted_Catholic") %in% names(pc)))
  pr <- predict(model_regression_not_prepped)
  expect_s3_class(pr, "predicted_df")
  expect_true(all(c("Fertility", "predicted_Fertility") %in% names(pr)))
  pm <- predict(model_multi_prepped)
  expect_s3_class(pm, "predicted_df")
  expect_true(all(c("Species", "predicted_Species") %in% names(pm)))
})

test_that("predict can handle binary character non Y/N columns", {
  training_data %>%
    dplyr::mutate(Catholic = ifelse(Catholic == "Y", "yes", "no")) %>%
    machine_learn(province, outcome = Catholic, tune = FALSE, models = "xgb") %>%
    predict() %>%
    expect_s3_class("predicted_df")
  training_data %>%
    dplyr::mutate(Catholic = factor(ifelse(Catholic == "Y", "cath", "other"))) %>%
    machine_learn(province, outcome = Catholic, tune = FALSE, models = "glm") %>%
    predict() %>%
    expect_s3_class("predicted_df")
})

test_that("predict handles new levels on model_list from prep_data", {
  expect_warning(preds <- predict(model_regression_prepped, test_data_newlevel),
                 "not observed in training")
  expect_s3_class(preds, "predicted_df")
})

test_that("predict handles missingness where unobserved in training prep_data", {
  expect_warning(preds <- predict(model_regression_prepped, test_data_new_missing))
  expect_s3_class(preds, "predicted_df")
})

test_that("predict doesn't need columns ignored in training", {
  predict(model_classify_prepped, dplyr::select(test_data, -province))
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

  capture_output(mes <- capture_messages(print(multi_prepped_prepped)))
  expect_false(stringr::str_detect(mes, "prepped"))
  expect_true(stringr::str_detect(mes, "predicted"))
})

test_that("printing predicted df prints the data frame", {
  out <- capture_output(print(regression_prepped_not))
  expect_true(stringr::str_detect(out, "tibble"))
})

test_that("printing classification df gets ROC/PR metric right", {
    roc <-
      training_data %>%
      prep_data(province, outcome = Catholic, make_dummies = TRUE) %>%
      flash_models(Catholic, models = "RF", metric = "ROC", n_folds = 2) %>%
      predict()
    pr <-
      training_data %>%
      prep_data(province, outcome = Catholic, make_dummies = TRUE) %>%
      flash_models(Catholic, models = "RF", metric = "PR", n_folds = 2) %>%
      predict()
  expect_false(stringr::str_detect(as.character(capture_message(print(roc))), "PR"))
  expect_false(stringr::str_detect(as.character(capture_message(print(pr))), "ROC"))
})

test_that("ready_with_prep preps appropriately", {
  prepped <- ready_with_prep(model_regression_prepped, test_data)
  expect_s3_class(prepped, "data.frame")
  prepped_predictors <- setdiff(names(prepped), attr(model_regression_prepped, "target"))
  # Expected predictors are numerics from rec$var_info, plus 3 of 4 dummied Catholics
  rec <- attr(model_regression_prepped, "recipe")
  predictors <-
    rec$var_info %>%
    dplyr::filter(role == "predictor" & type == "numeric") %>%
    dplyr::pull(variable) %>%
    c(paste0("Catholic", "_", c("Y", "other", "missing")))
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
  d$x3 <- purrr::map_chr(d$y, ~ sample(c("a", "b"), 1, FALSE, if (.x) c(2, 1) else c(1, 2)))
  pd <- prep_data(d, outcome = y)
  # Default Y is positive
  preds <- list(
    tm_rf = pd %>% flash_models(y, models = "rf") %>% predict(),
    tm_xgb = pd %>% flash_models(y, models = "xgb") %>% predict(),
    ml = machine_learn(d, outcome = y, models = "glm", tune = FALSE) %>% predict()
  )
  expect_true(all(
    purrr::map_lgl(preds, ~ {
      mean(.x$predicted_y[.x$y == "Y"]) >= mean(.x$predicted_y[!.x$y == "N"])
    })
  ))
  # Set N as positive
  preds <- list(
    tm_rf = pd %>% flash_models(y, n_folds = 2, models = "rf", positive_class = "N") %>% predict(),
    tm_xgb = pd %>% flash_models(y, n_folds = 2, models = "xgb", positive_class = "N") %>% predict(),
    ml = machine_learn(d, outcome = y, models = "rf", tune_depth = 2, positive_class = "N") %>% predict()
  )
  expect_true(all(
    purrr::map_lgl(preds, ~ {
      mean(.x$predicted_y[.x$y == "Y"]) <= mean(.x$predicted_y[!.x$y == "N"])
    })
  ))
})

test_that("can predict on untuned classification model with new data", {
  d <- na.omit(pima_diabetes)[1:100, ]
  dtest <- na.omit(pima_diabetes)[101:110, ]
  c_models <- machine_learn(d, patient_id, outcome = diabetes, tune = FALSE, n_folds = 2)
  c_preds_test <- predict(c_models, dtest)
  expect_s3_class(c_preds_test, "predicted_df")
})

test_that("can predict on untuned multiclass model with new data", {
  m_models <- machine_learn(m_train, outcome = Species, tune = FALSE, n_folds = 2)
  m_preds_test <- predict(m_models, m_test)
  expect_s3_class(m_preds_test, "predicted_df")
})

test_that("predict without new data returns out of fold predictions from training", {
  preds <- predict(model_classify_prepped)$predicted_Catholic
  oofpreds <- dplyr::arrange(model_classify_prepped[[extract_model_info(model_classify_prepped)$best_model_name]]$pred, rowIndex)$Y
  expect_true(all.equal(preds, oofpreds))
})

test_that("get_oof_predictions seems to work", {
  oof_mrp <- get_oof_predictions(model_regression_prepped)
  oof_mcp <- get_oof_predictions(model_classify_prepped)
  oof_mrn <- get_oof_predictions(model_regression_not_prepped)
  oof_mmp <- get_oof_predictions(model_multi_prepped)
  expect_s3_class(oof_mcp, "tbl_df")
  expect_true(is.numeric(oof_mrp$preds))
  expect_true(is.numeric(oof_mcp$preds))
  expect_true(is.numeric(oof_mrn$preds))
  expect_true(is.factor(oof_mmp$preds))
  expect_true(is.numeric(oof_mrp$outcomes))
  expect_false(is.numeric(oof_mcp$outcomes))
  expect_false(is.numeric(oof_mmp$outcomes))
  expect_setequal(as.character(oof_mcp$outcomes), c("N", "Y"))
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
  expect_true(tibble::is_tibble(get_pred_summary(classification_prepped_prepped)))
  expect_equal(dim(get_pred_summary(classification_prepped_prepped)), c(1, 6))
})

test_that("predict xgboost works when columns are in different order", {
  xgb_mod <- model_classify_not_prepped["eXtreme Gradient Boosting"]
  same_order <-
    test_data %>%
    dplyr::select(-province) %>%
    predict(xgb_mod, .)
  new_order <-
    test_data %>%
    dplyr::select(-province) %>%
    dplyr::select(Agriculture, Education, dplyr::everything()) %>%
    predict(xgb_mod, .)
  expect_equal(same_order[, order(names(same_order))],
               new_order[, order(names(new_order))])
})

test_that("print.predicted_df works", {
  pred_mes <- capture_messages({
    pred_print <- capture_output(classification_prepped_prepped, TRUE)
  })
  expect_true(stringr::str_detect(pred_mes, "predicted by"))
  expect_true(stringr::str_detect(pred_print, "A tibble"))
})

test_that("print.predicted_df works when attributes are stripped", {
  stripped <- structure(regression_prepped_not, model_info = NULL)
  expect_error(capture_output(stripped, TRUE), NA)
})

test_that("print.predicted_df works after join", {
  joined <- dplyr::inner_join(classification_not_not,
                              classification_not_not,
                              by = "province")
  expect_error(capture_output(joined, TRUE), NA)
})

test_that("multiclass errors with groups", {
  expect_warning(pr <- predict(model_multi_prepped, risk_groups = 4))
  expect_s3_class(pr, "predicted_df")
  expect_warning(pr <- predict(model_multi_prepped, outcome_groups = 4))
  expect_s3_class(pr, "predicted_df")
})

test_that("risk_groups works on training data", {
  rg5 <- predict(model_classify_prepped, risk_groups = 5)
  expect_equal(length(unique(rg5$predicted_group)), 5)
  gr <- c("low", "medium", "high")
  rg_custom <- predict(model_classify_prepped, risk_groups = gr)
  expect_setequal(as.character(rg_custom$predicted_group), gr)
  expect_equal(
    sum(rg_custom$predicted_group == "high"), sum(rg_custom$predicted_group == "low"),
    tolerance = 1)
  expect_true(with(rg_custom,
                   min(predicted_Catholic[predicted_group == "high"]) >=
                     max(predicted_Catholic[predicted_group == "medium"])))
  expect_true(all(c("group_type", "cutpoints") %in% names(attributes(rg_custom$predicted_group))))
})

test_that("risk_groups works on test data", {
  rg5 <- predict(model_classify_prepped, test_data, risk_groups = 5)
  expect_false(any(is.na(rg5$predicted_group)))
  gr <- c("v low", "low", "high", "v high")
  rg_custom <- predict(model_classify_prepped, test_data, risk_groups = gr)
  expect_true(all(as.character(rg_custom$predicted_group) %in% gr))
  expect_true(with(rg_custom,
                   min(predicted_Catholic[stringr::str_detect(predicted_group, "high")]) >=
                     min(predicted_Catholic[stringr::str_detect(predicted_group, "low")])
  ))
  expect_true(all(c("group_type", "cutpoints") %in% names(attributes(rg_custom$predicted_group))))

  grps <- c(low = 20, mid = 2, high = 1)
  rg_sized <- predict(model_classify_prepped, test_data, risk_groups = grps)
  expect_false(any(is.na(rg_sized$predicted_group)))
  expect_true(all(as.character(rg_sized$predicted_group) %in% names(grps)))
  expect_true(with(rg_sized, sum(predicted_group == "low") > sum(predicted_group == "high")))
})

test_that("outcome_groups works on training data", {
  cg <- predict(model_classify_prepped, outcome_groups = TRUE)$predicted_group
  expect_equal(levels(cg), c("N", "Y"))
  expect_setequal(test_data$Catholic, as.character(cg))
  fp_cheap <- predict(model_classify_prepped, outcome_groups = 10)$predicted_group
  fp_expensive <- predict(model_classify_prepped, outcome_groups = .1)$predicted_group
  expect_true(sum(fp_cheap == "Y") >= sum(cg == "Y"))
  expect_true(sum(cg == "Y") >= sum(fp_expensive == "Y"))
})

test_that("outcome_groups works on test data", {
  cg <- predict(model_classify_prepped, test_data, outcome_groups = TRUE)$predicted_group
  expect_true(all(c("Y", "N") %in% as.character(cg)))
  fp_cheap <- predict(model_classify_prepped, test_data, outcome_groups = 10)$predicted_group
  fp_expensive <- predict(model_classify_prepped, test_data, outcome_groups = .1)$predicted_group
  expect_true(sum(fp_cheap == "Y") >= sum(cg == "Y"))
  expect_true(sum(cg == "Y") >= sum(fp_expensive == "Y"))
})

test_that("add_groups errors informatively", {
  expect_error(predict(model_classify_prepped, risk_groups = 5, outcome_groups = TRUE),
               "cbind")
  expect_error(predict(model_classify_prepped, outcome_groups = FALSE), "FALSE")
})

test_that("get_cutoffs", {
  og <- predict(model_classify_prepped, outcome_groups = 2)
  rg <- predict(model_classify_prepped, risk_groups = 5)
  og_mes <- capture_messages(og_cutoffs <- get_cutoffs(og))
  junk <- capture_output(rg_mes <- capture_messages(rg_cutoffs <- get_cutoffs(rg)))
  expect_true(stringr::str_detect(tolower(og_mes), "outcome"))
  expect_true(stringr::str_detect(tolower(rg_mes), "risk"))
  expect_true(is.numeric(og_cutoffs))
  expect_length(og_cutoffs, 1)
  expect_s3_class(rg_cutoffs, "data.frame")
  expect_equal(stringr::str_extract(rg_cutoffs$group, "[0-9]$"), as.character(1:5))
  expect_true(all(diff(rg_cutoffs$minimum_probability) < 0))
})

test_that("predict bakes 0/1 outcomes", {
  m <- pima_diabetes %>%
    dplyr::slice(1:50) %>%
    mutate(diabetes = ifelse(diabetes == "Y", 1, 0)) %>%
    machine_learn(patient_id, outcome = diabetes, tune = FALSE, models = "glm")
  expect_setequal(attr(m, "recipe")$orig_data$diabetes, 0:1)
  p <- predict(m, outcome_groups = TRUE)
  expect_setequal(as.character(get_oof_predictions(m)$outcomes), c("N", "Y"))
  expect_setequal(as.character(p$diabetes), c("N", "Y"))
  expect_setequal(as.character(p$predicted_group), c("N", "Y"))

  new_data <- pima_diabetes %>%
    dplyr::slice(51:65) %>%
    mutate(diabetes = ifelse(diabetes == "Y", 1, 0))
  pnew <- predict(m, newdata = new_data, outcome_groups = TRUE)
  expect_setequal(as.character(pnew$diabetes), c("N", "Y"))
  expect_setequal(as.character(pnew$predicted_group), c("N", "Y"))
})

test_that("Predict relevels outcome", {
  d <- split_train_test(pima_diabetes, outcome = diabetes)
  m <- machine_learn(d$train, patient_id, outcome = diabetes, models = "glm",
                     tune = FALSE)

  predictions <- predict(m, newdata = d$test)
  actual_levels <- levels(predictions$diabetes)
  pos_class <- attr(m, "positive_class")
  expect_equal(actual_levels[1], pos_class)
})

test_that("predict empty template/orig_data no error", {
  attr(model_classify_prepped, "recipe")$template <- NULL
  expect_error(predict(model_classify_prepped), NA)

  attr(model_classify_prepped, "recipe")$orig_data <- NULL
  expect_error(predict(model_classify_prepped), NA)
})

test_that("predict returns accurate has_training_data, and print.predicted_df", {
  # True when normal
  pred <- predict(model_classify_prepped)
  expect_true(attr(pred, "model_info")$has_training_data)
  capture_output(out <- capture_messages(print(pred)))
  expect_false(stringr::str_detect(out, "Your model was sanitized of PHI"))

  # True when only one orig or template
  tmp <- attr(model_classify_prepped, "recipe")$orig_data
  attr(model_classify_prepped, "recipe")$orig_data <- NULL
  pred <- predict(model_classify_prepped)
  expect_true(attr(pred, "model_info")$has_training_data)

  attr(model_classify_prepped, "recipe")$orig_data <- tmp
  attr(model_classify_prepped, "recipe")$template <- NULL
  pred <- predict(model_classify_prepped)
  expect_true(attr(pred, "model_info")$has_training_data)

  # False when template is NULL, and no other data
  attr(model_classify_prepped, "recipe")$orig_data <- NULL
  attr(model_classify_prepped, "recipe")$tempate <- NULL
  pred <- predict(model_classify_prepped)
  expect_false(attr(pred, "model_info")$has_training_data)
  capture_output(out <- capture_messages(print(pred)))
  expect_true(stringr::str_detect(out, "Your model was sanitized of PHI"))

  # True when template is NULL, and other data
  pred <- predict(model_classify_prepped, training_data)
  expect_true(attr(pred, "model_info")$has_training_data)
  capture_output(out <- capture_messages(print(pred)))
  expect_false(stringr::str_detect(out, "Your model was sanitized of PHI"))
})


remove_logfiles()
