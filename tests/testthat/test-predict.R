context("Test predict")

### Setup. Data from caret.
set.seed(2570)
library(magrittr)
data("swiss")
swiss <-
  swiss %>%
  tibble::rownames_to_column("province") %>%
  dplyr::mutate(Catholic = ifelse(Catholic > 80, "Y", "N")) %>%
  tibble::as_tibble()
part <- caret::createDataPartition(swiss$Catholic, .8)[[1]]
training_data <- swiss[part, ]
test_data <- swiss[-part, ]
test_data_newlevel <- test_data
test_data_newlevel$Catholic[c(3, 7, 16)] <- "unobserved in training"
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
  predict(model_regression_not_prepped, test_data)
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
  expect_s3_class(pc, "hcai_predicted_df")
  expect_true(all(c("Catholic", "predicted_Catholic") %in% names(pc)))
  pr <- predict(model_regression_not_prepped)
  expect_s3_class(pr, "hcai_predicted_df")
  expect_true(all(c("Fertility", "predicted_Fertility") %in% names(pr)))
})

test_that("predict can handle binary character non Y/N columns", {
  training_data %>%
    dplyr::mutate(Catholic = ifelse(Catholic == "Y", "yes", "no")) %>%
    machine_learn(Catholic) %>%
    predict() %>%
    expect_s3_class("data.frame")
  training_data %>%
    dplyr::mutate(Catholic = factor(ifelse(Catholic == "Y", "cath", "other"))) %>%
    machine_learn(Catholic) %>%
    predict() %>%
    expect_s3_class("data.frame")
})

test_that("predict handles new levels on model_list from prep_data", {
  expect_s3_class(predict(model_regression_prepped, test_data_newlevel),
                  "hcai_predicted_df")
})

test_that("predict handles missingness where unobserved in training prep_data", {
  expect_s3_class(predict(model_regression_prepped, test_data_new_missing),
                  "hcai_predicted_df")
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
  roc <-
    training_data %>%
    prep_data(province, outcome = Catholic, make_dummies = TRUE) %>%
    tune_models(Catholic, models = "RF", metric = "ROC", tune_depth = 2) %>%
    predict()
  pr <-
    training_data %>%
    prep_data(province, outcome = Catholic, make_dummies = TRUE) %>%
    tune_models(Catholic, models = "RF", metric = "PR", tune_depth = 2) %>%
    predict()
  expect_true(stringr::str_detect(capture_message(print(roc)), "ROC"))
  expect_true(stringr::str_detect(capture_message(print(pr)), "PR"))
})
