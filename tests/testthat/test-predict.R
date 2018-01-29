context("Test predict")

### Setup. Data from caret.
library(magrittr)
data("swiss")
swiss <-
  swiss %>%
  tibble::rownames_to_column("province") %>%
  dplyr::mutate(Catholic = ifelse(Catholic > 80, "Y", "N")) %>%
  tibble::as_tibble()
# Temp: add bogus category column so prep_data doesn't freak out
swiss$bogus <- sample(c("a", "b"), nrow(swiss), TRUE)
part <- caret::createDataPartition(swiss$Catholic, .8)[[1]]
tr <- swiss[part, ]
te <- swiss[-part, ]
# Need to test combos of:
# reg/class x d from prep_data/not x newdata from prep_data/not
## Except training data not prepped and newdata prepped; that shouldn't work
# Format: m - r = reg, c = class - p = prepped, n = not
mcp <-
  tr %>%
  prep_data(province, Catholic, dummies = TRUE) %>%
  dplyr::select(-province) %>%
  tune_models(Catholic)
mcn <-
  tr %>%
  dplyr::select(-province) %>%
  tune_models(Catholic)
suppressWarnings({  # Warning when RF can't find a good cut # nolint
  mrp <-
    tr %>%
    prep_data(province, Fertility, dummies = TRUE) %>%
    dplyr::select(-province) %>%
    tune_models(Fertility)
  mrn <-
    tr %>%
    dplyr::select(-province) %>%
    tune_models(Fertility)
})
# And prepped newdata to go with them
te_reg_prep <- prep_data(te, rec_obj = mrp)
te_class_prep <- prep_data(te, rec_obj = mcp)

# Output. Format: m - r = reg, c = class - n = not prepped training, p = prepped training - n = not prepped test, p = prepped test  # nolint
prpn <- predict(mrp, te)
prpp <- predict(mrp, te_reg_prep)
prnn <- predict(mrn, te)

pcpn <- predict(mcp, te)
pcpp <- predict(mcp, te_class_prep)
pcnn <- predict(mcn, te)

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
  expect_true(all.equal(prpn$predicted_Fertility, prpp$predicted_Fertility, prnn$predicted_Fertility))
  expect_true(all.equal(pcpn$predicted_Catholic, pcpp$predicted_Catholic, pcnn$predicted_Catholic))
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
