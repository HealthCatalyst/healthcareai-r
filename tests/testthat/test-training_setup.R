context("checking training setup")

test_that("check_metric when correct, doesn't throw error or change metric", {
  expect_warning(metric <- check_metric("classification", "PR"), NA)
  expect_equal(metric, "PR")

  expect_warning(metric <- check_metric("regression", "RMSE"), NA)
  expect_equal(metric, "RMSE")
})

test_that("check_metric when incorrect, throws error or throws warnings and corrects", {
  expect_warning(new_metric <- check_metric("classification", "MAE"))
  expect_equal(new_metric, "ROC")

  expect_warning(new_metric <- check_metric("regression", "PR"))
  expect_equal(new_metric, "RMSE")

  expect_error(check_metric("garbage", ""))
})

test_that("set_default_metric returns correct metric", {
  expect_equal(set_default_metric("regression"), "RMSE")
  expect_equal(set_default_metric("classification"), "ROC")

  expect_error(set_default_metric("garbage"), "Don't have")
})
