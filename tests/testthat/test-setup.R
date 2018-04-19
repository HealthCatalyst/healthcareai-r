# Test functions in R/models.R
context("checking modeling setup functions")

test_that("translate_model_names works", {
  expect_equal(translate_model_names("rf"), "ranger")
  expect_equal(translate_model_names("knn"), "kknn")
  expect_equal(translate_model_names(c("knn", "rf")), c("kknn", "ranger"))
})

test_that("translate_model_names works backwards", {
  expect_equal(translate_model_names("ranger"), "rf")
  expect_equal(translate_model_names("kknn"), "knn")
  expect_equal(translate_model_names(c("kknn", "ranger")), c("knn", "rf"))
})
