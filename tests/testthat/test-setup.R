# Test functions in R/models.R
context("checking modeling setup functions")

test_that("translate_model_names works", {
  expect_equal(translate_model_names("rf"), "ranger")
  expect_equal(translate_model_names("xgb"), "xgbTree")
  expect_equal(translate_model_names(c("xgb", "rf")), c("xgbTree", "ranger"))
})

test_that("translate_model_names works backwards", {
  expect_equal(translate_model_names("ranger"), "rf")
  expect_equal(translate_model_names("xgbTree"), "xgb")
  expect_equal(translate_model_names(c("xgbTree", "ranger")), c("xgb", "rf"))
})
