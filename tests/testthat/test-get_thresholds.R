context("get_thresholds")

set.seed(2507)
m <- mtcars %>%
  machine_learn(outcome = am, tune = FALSE, models = "xgb")
pred <- predict(m, dplyr::sample_n(mtcars, 5))
def <- get_thresholds(pred)
opt_cost <- get_thresholds(pred, optimize = "cost")

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
  expect_false("fnr" %in% names(get_thresholds(m, measures = "acc")))
  expect_false("fnr" %in% names(get_thresholds(pred, measures = "acc")))
  expect_error(get_thresholds(m, measures = c("mi", "f")), "not available")
})

test_that("cost_fp and cost_fn are respected", {
  set1 <- get_thresholds(pred, cost_fn = 10)
  set2 <- get_thresholds(pred, measures = "cost", cost_fp = .1, cost_fn = 3)
  expect_false(isTRUE(all.equal(def, set1)))
  expect_false(isTRUE(all.equal(set1, set2)))
})

test_that("get_thresholds order rows by increasing threshold", {
  expect_true(all(diff(def$threshold) < 0))
  acc <- get_thresholds(m, measures = "acc")
  expect_true(all(diff(acc$threshold) < 0))
})

test_that("get_thresholds return has thresholds_df class", {
  expect_s3_class(def, "thresholds_df")
})

test_that("get_thresholds errors nicely if labels aren't present", {
  expect_error(get_thresholds(select(pred, -am)), "doesn't have outcomes")
})

test_that("optimize at default doesn't return `optimal` column", {
  expect_false("optimal" %in% names(def))
})

test_that("get_thresholds errors informatively if optimize not in measures", {
  expect_error(get_thresholds(pred, optimize = "cost", measures = c("ppv", "npv")),
               "must be one of the measures")
})

test_that("optimize cost returns a column with TRUE only in lowest-cost row", {
  expect_true("optimal" %in% names(opt_cost))
  expect_equal(sum(opt_cost$optimal), 1)
  expect_equal(which(opt_cost$optimal), which.min(opt_cost$cost))
})

test_that("optimize attaches attribute with name of optimized measure", {
  expect_null(attr(def, "optimized"))
  expect_equal(attr(opt_cost, "optimized"), "cost")
})

test_that("get_measures", {
  expect_error(get_measures("nope"), "not available")
  get_all <- get_measures("all")
  expect_true("cost" %in% names(get_all))
  expect_setequal(get_all, c(-1, 1))
  to_get <- c("acc", "tpr")
  expect_equal(names(get_measures(to_get)), to_get)
})

#######
context("plot.thresholds_df")

test_that("plot.thresholds_df is a registered S3 method", {
  expect_true("plot.thresholds_df" %in% methods("plot"))
})

test_that("plot.thresholds_df returns a ggplot", {
  expect_s3_class(plot(def, print = FALSE), "gg")
})

test_that("plot.thresholds_df respects optimize", {
  expect_false(isTRUE(all.equal(
    plot(def, print = FALSE), plot(opt_cost, print = FALSE)
  )))
})
