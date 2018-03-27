context("Checking prediction plots")

### Setup
d <- prep_data(pima_diabetes[1:100, ], patient_id, outcome = age)
m <- tune_models(d, outcome = age, n_folds = 2, tune_depth = 1, models = "rf")
reg_preds_self <- predict(m)
reg_preds_new <- predict(m, pima_diabetes[101:110, ])

### Tests
test_that("plot.hcai_predicted_df stops if there's no outcome", {
  expect_error(plot(dplyr::select(reg_preds_self, -age)), "outcome")
  expect_error(plot(dplyr::select(reg_preds_new, -age)), "outcome")
})

test_that("plot.hcai_predicted_df stops if outcome vector is wrong length or class", {
  expect_error(plot(dplyr::select(reg_preds_self, -age), outcomes = 1:5),
               "length")
  expect_error(plot(dplyr::select(reg_preds_self, -age),
                    outcomes = sample(letters, nrow(reg_preds_self), TRUE)),
               "class")
})

test_that("plot.hcai_predicted_df warns but works if outcomes present in df and passed in", {
  expect_warning(p <- plot(reg_preds_new, 1:10, print = FALSE), "outcome")
  expect_s3_class(p, "gg")
})

test_that("plot_regression_predictions handles defaults", {
  expect_s3_class(plot(reg_preds_new, print = FALSE), "gg")
  expect_s3_class(plot(reg_preds_self, print = FALSE), "gg")
})

test_that("plot_regression_predictions handles separately supplied outcomes", {
  expect_s3_class(plot(dplyr::select(reg_preds_self, -age),
                       outcomes = reg_preds_self$age, print = FALSE),
                  "gg")
})
