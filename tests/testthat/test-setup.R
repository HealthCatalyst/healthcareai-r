# Test functions in R/models.R
context("checking modeling setup functions")

test_that("get_hyperparameter_defaults works", {
  hp <- get_hyperparameter_defaults(c("rf", "knn"), 100, 10, "classification")
  expect_equal(2, length(hp))
  expect_equal(class(hp), "list")
  expect_equal(class(hp[[1]]), "list")
  expect_setequal(names(hp), c("rf", "knn"))

  hp <- get_hyperparameter_defaults("knn", 1000, 2, "regression")
  expect_equal(1, length(hp))
  expect_equal(class(hp), "list")
  expect_equal(class(hp[[1]]), "list")
  expect_equal(names(hp), "knn")
})

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


get_hyperparameter_defaults(c("rf", "knn"), 100, 10, "classification")
