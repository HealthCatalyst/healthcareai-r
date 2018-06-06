context("Checking interpret")

set.seed(271)
m <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = diabetes)
g <- m["glmnet"]

test_that("interpret returns a tbl w/child class coefs if glmnet is in model_list", {
  suppressWarnings(expect_s3_class(interpret(m), "tbl_df"))
  expect_s3_class(interpret(g), "tbl_df")
  expect_s3_class(interpret(g), "coefs")
})

test_that("interpret errors with ref to var-imp if no glmnet present", {
  expect_error(interpret(m["Random Forest"]), "get_variable_importance")
  expect_error(interpret(m[which(names(m) != "glmnet")]), "get_variable_importance")
})

test_that("interpret errors informatively if sparsity isn't number in 1-100", {
  expect_error(interpret(g, sparsity = -2), "sparsity")
  expect_error(interpret(g, sparsity = 7), "sparsity")
  expect_error(interpret(g, sparsity = "lalala"), "sparsity")
})

test_that("interpret respects sparsity and returns sparser coefs when larger", {
  expect_false(isTRUE(all.equal(interpret(g), interpret(g, 0))))
  i10 <- interpret(g, .1)
  i90 <- interpret(g, .9)
  mean_abs_beta <- function(i) mean(abs(dplyr::filter(i, variable != "(Intercept)")$coefficient))
  expect_true(mean_abs_beta(i90) > mean_abs_beta(i10))
  reg_m <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = plasma_glucose, models = "glm")
  expect_true(mean_abs_beta(interpret(reg_m, .75)) > mean_abs_beta(interpret(reg_m, .25)))
})

test_that("interpret respects remove_zeros", {
  default <- interpret(g)
  removed <- interpret(g, remove_zeros = TRUE)
  not_removed <- interpret(g, remove_zeros = FALSE)
  expect_equal(default, removed)
  expect_false(any(removed$coefficient == 0))
  expect_true(nrow(removed) < nrow(not_removed))
})

test_that("interpret pulls lambda correctly", {
  default <- interpret(g)
  sparse <- interpret(g, sparsity = 1)
  loose <- interpret(g, sparsity = 0)
  expect_true(attr(default, "lambda") < attr(loose, "lambda"))
  expect_true(attr(sparse, "lambda") < attr(default, "lambda"))
})
