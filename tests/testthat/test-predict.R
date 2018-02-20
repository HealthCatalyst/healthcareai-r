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
tr <- swiss[part, ]
te <- swiss[-part, ]
te_newlevel <- te
te_newlevel$Catholic[c(3, 7, 16)] <- "unobserved in training"
te_newlevel$Catholic[5] <- "another new level"
te_new_missing <- te
te_new_missing$Agriculture[1:3] <- NA
te_new_missing$Catholic[6:7] <- NA
# Need to test combos of:
# reg/class x d from prep_data/not x newdata from prep_data/not
## Except training data not prepped and newdata prepped; that shouldn't work
# Format: m - r = reg, c = class - p = prepped, n = not
mcp <-
  tr %>%
  prep_data(province, outcome = Catholic, make_dummies = TRUE) %>%
  tune_models(Catholic)
mcn <-
  tr %>%
  dplyr::select(-province) %>%
  tune_models(Catholic)
# Warning when RF can't find a good cut
suppressWarnings({
  mrp <-
    tr %>%
    prep_data(province, outcome = Fertility, make_dummies = TRUE) %>%
    tune_models(Fertility)
  mrn <-
    tr %>%
    dplyr::select(-province) %>%
    tune_models(Fertility)
})
# And prepped newdata to go with them
te_reg_prep <- prep_data(te, recipe = mrp)
te_class_prep <- prep_data(te, recipe = mcp)

# Output. Format: m - r = reg, c = class - n = not prepped training, p = prepped training - n = not prepped test, p = prepped test  # nolint
prpp <- predict(mrp, te_reg_prep)
pcpp <- predict(mcp, te_class_prep)

prnn <- predict(mrn, te)
pcnn <- predict(mcn, te)

prpn <- predict(mrp, te)
suppressWarnings(pcpn <- predict(mcp, te))

# Test for some messages and warnings when whether to prep before predict is unclear:
# Here, should get a warning because all predictors are numeric, so they appear
# to have been prepped even when they haven't, which triggers warning.
suppressWarnings({
  pcpn_message <- capture_message( predict(mcp, te) )
})
prpn_message <- capture_message( predict(mrp, te) )
pcpn_warning <- capture_warning( predict(mcp, te) )

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
  pc <- predict(mcp)
  expect_s3_class(pc, "hcai_predicted_df")
  expect_true(all(c("Catholic", "predicted_Catholic") %in% names(pc)))
  pr <- predict(mrn)
  expect_s3_class(pr, "hcai_predicted_df")
  expect_true(all(c("Fertility", "predicted_Fertility") %in% names(pr)))
})

test_that("predict can handle binary character non Y/N columns", {
  tr %>%
    dplyr::mutate(Catholic = ifelse(Catholic == "Y", "yes", "no")) %>%
    machine_learn(Catholic) %>%
    predict() %>%
    expect_s3_class("data.frame")
  tr %>%
    dplyr::mutate(Catholic = factor(ifelse(Catholic == "Y", "cath", "other"))) %>%
    machine_learn(Catholic) %>%
    predict() %>%
    expect_s3_class("data.frame")
})

test_that(paste0("predict errors informatively when new levels are observed in",
                 "newdata and training data was unprepped"), {
  expect_error(predict(mrn, te_newlevel), regexp = "Catholic")
})

test_that(paste0("predict errors informatively when missingness on newdata and",
                 "training data was unprepped"), {
  expect_error(predict(mrn, te_new_missing),
               regexp = "Catholic, Agriculture")
})

test_that("predict errors if prepdata TRUE but no recipe there", {
  expect_error(predict(mrn, te_newlevel, prepdata = T), regexp = "prep")
})
