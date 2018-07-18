context("get_thresholds")

set.seed(2507)
m <- mtcars %>%
  machine_learn(outcome = am, tune = FALSE, models = "xgb")
pred <- predict(m, dplyr::sample_n(mtcars, 5), prepdata = TRUE)
def <- get_thresholds(pred)

test_that("get_thresholds works for model_list", {
  expect_s3_class(get_thresholds(m), "tbl_df")
})

test_that("get_thresholds works for predicted_df", {
  expect_s3_class(def, "tbl_df")
})

test_that("number of rows is right", {
  expect_equal(nrow(get_thresholds(m)),
               length(unique(predict(m)$predicted_am)) + 1)
  expect_equal(nrow(def),
               length(unique(pred$predicted_am)) + 1)
})

test_that("measures are respected", {
  expect_false("fnr" %in% names(get_thresholds(m, "acc")))
  expect_false("fnr" %in% names(get_thresholds(pred, "acc")))
  alt_metrics <- c("mi", "f")
  expect_true(all(alt_metrics %in% names(get_thresholds(m, alt_metrics))))
})

test_that("cost.fp and cost.fn are respected", {
  set1 <- get_thresholds(pred, cost.fn = 10)
  set2 <- get_thresholds(pred, "cost", cost.fp = .1, cost.fn = 3)
  expect_false(isTRUE(all.equal(def, set1)))
  expect_false(isTRUE(all.equal(set1, set2)))
})

test_that("get_thresholds order rows by increasing threshold", {
  expect_true(all(diff(def$threshold) < 0))
  acc <- get_thresholds(m, "acc")
  expect_true(all(diff(acc$threshold) < 0))
})

test_that("get_thresholds errors gracefully if a scaler measure is provided", {
  expect_error(get_thresholds(m, "auc"), "one value per threshold")
})

test_that("get_thresholds return has thresholds_df class", {
  expect_s3_class(def, "thresholds_df")
})


#######
context("plot.thresholds_df")

test_that("plot.thresholds_df is a registered S3 method", {
  expect_true("plot.thresholds_df" %in% methods("plot"))
})

test_that("plot.thresholds_df returns a ggplot", {
  expect_s3_class(plot(def, print = FALSE), "gg")
})
