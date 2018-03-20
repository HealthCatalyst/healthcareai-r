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

# Output. Format: m - r = reg, c = class - n = not prepped training, p = prepped training - n = not prepped test, p = prepped test  # nolint
prpp <- predict(model_regression_prepped, test_data_reg_prep)
pcpp <- predict(model_classify_prepped, test_data_class_prep)

prnn <- predict(model_regression_not_prepped, test_data)
pcnn <- predict(model_classify_not_prepped, test_data)

prpn <- predict(model_regression_prepped, test_data)
suppressWarnings(pcpn <- predict(model_classify_prepped, test_data))

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
  expect_s3_class(prpn, "tbl_df")
  expect_s3_class(prpp, "tbl_df")
  expect_s3_class(prnn, "tbl_df")
})

test_that("predict classification returns a tibble", {
  expect_s3_class(pcpn, "tbl_df")
  expect_s3_class(pcpp, "tbl_df")
  expect_s3_class(pcnn, "tbl_df")
})

test_that("prepping data inside or before predict produces same output", {
  expect_true(all.equal(prpn$predicted_Fertility, prpp$predicted_Fertility))
  expect_true(all.equal(pcpn$predicted_Catholic, pcpp$predicted_Catholic))
})

test_that("predictions are better than chance", {
  # Classification: predicted probs for actual Ys are greater than for actual Ns
  pcpn %>%
    dplyr::group_by(Catholic) %>%
    dplyr::summarize(mean_predicted_prob = mean(predicted_Catholic)) %>%
    with(., mean_predicted_prob[Catholic == "Y"] >
           mean_predicted_prob[Catholic == "N"]) %>%
    expect_true()
  # Regression: residuals are less than mean prediction
  with(prpn, mean(abs(predicted_Fertility - Fertility)) <
         mean(abs(mean(Fertility) - Fertility))) %>%
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
    machine_learn(outcome = Catholic) %>%
    predict() %>%
    expect_s3_class("data.frame")
  training_data %>%
    dplyr::mutate(Catholic = factor(ifelse(Catholic == "Y", "cath", "other"))) %>%
    machine_learn(outcome = Catholic) %>%
    predict() %>%
    expect_s3_class("data.frame")
})

test_that("predict handles new levels on model_list from prep_data", {
  expect_s3_class(predict(model_regression_prepped, test_data_newlevel), "hcai_predicted_df")
})

test_that("predict handles missingness where unobserved in training prep_data", {
  expect_warning(preds <- predict(model_regression_prepped, test_data_new_missing))
  expect_s3_class(preds, "hcai_predicted_df")
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

test_that("Warnings are issued if there is new missingness in predict", {
  expect_warning(predict(model_classify_prepped, test_data_new_missing),
                 "Agriculture")
  expect_warning(predict(model_regression_prepped, test_data_new_missing),
                 "Agriculture")
})

# What about ID columns?
