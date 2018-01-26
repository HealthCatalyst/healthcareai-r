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
mrp <-
  swiss %>%
  prep_data(province, Catholic, dummies = TRUE) %>%
  dplyr::select(-province) %>%
  tune_models(Catholic)
mrn <-
  swiss %>%
  dplyr::select(-province) %>%
  tune_models(Catholic)
mcp <-
  swiss %>%
  prep_data(province, Fertility, dummies = TRUE) %>%
  dplyr::select(-province) %>%
  tune_models(Fertility)
mcn <-
  swiss %>%
  dplyr::select(-province) %>%
  tune_models(Fertility)
# And prepped newdata to go with them
te_reg_prep <- prep_data(te, rec_obj = mrp)
te_class_prep <- prep_data(te, rec_obj = mcp)

test_that("predict.regression_list returns a tibble", {
  expect_s3_class(predict(mrp, te), "tbl_df")
  expect_s3_class(predict(mrp, te_reg_prep), "tbl_df")
  expect_s3_class(predict(mrn, te), "tbl_df")
})
test_that("predict.classification_list returns a tibble", {
  expect_s3_class(predict(mcp, te), "tbl_df")
  expect_s3_class(predict(mcp, te_class_prep), "tbl_df")
  expect_s3_class(predict(mcn, te), "tbl_df")
})
