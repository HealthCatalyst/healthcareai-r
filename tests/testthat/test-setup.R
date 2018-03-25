# Test functions in R/models.R
context("checking modeling setup functions")

test_that("get_hyperparameter_defaults works", {
  expect_equal(class(get_hyperparameter_defaults()), "list")
  expect_equal(2, length(get_hyperparameter_defaults()))
  expect_equal(1, length(get_hyperparameter_defaults("rf")))
})

test_that("translate_model_names works", {
  expect_equal(translate_model_names("rf"), "ranger")
  expect_equal(translate_model_names("knn"), "kknn")
  expect_equal(translate_model_names(c("knn", "rf")), c("kknn", "ranger"))

})
