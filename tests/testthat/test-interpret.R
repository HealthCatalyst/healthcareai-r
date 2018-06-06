context("Checking interpret")

set.seed(271)
m <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = diabetes)
g <- m["glmnet"]

test_that("interpret returns a tbl w/child class coefs if glmnet is in model_list", {
  suppressWarnings(expect_s3_class(interpret(m), "tbl_df"))
  expect_s3_class(interpret(g), "tbl_df")
  expect_s3_class(interpret(g), "interpret")
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


context("Checking plot.interpret")  # ------------------------------------------

test_that("plot.interpret is registered generic", {
  expect_true("plot.interpret" %in% methods("plot"))
})

test_that("plot.interpret returns a ggplot", {
  expect_s3_class(plot(interpret(g), print = FALSE), "gg")
  expect_s3_class(plot(interpret(g, 0, FALSE), print = FALSE), "gg")
  expect_s3_class(plot(interpret(g), caption = "none", title = "VI",
                       font_size = 16, print = FALSE), "gg")
})

test_that("plot.interpet seems to respect options", {
  ig <- interpret(g, remove_zeros = FALSE)
  def <- plot(ig, print = FALSE)
  keep_int <- plot(ig, include_intercept = TRUE, print = FALSE)
  no_zero <- plot(ig, remove_zeros = TRUE, print = FALSE)
  cust <- plot(ig, caption = "my custom caption", print = FALSE)
  expect_false(isTRUE(all.equal(def, keep_int)))
  expect_false(isTRUE(all.equal(def, no_zero)))
  expect_false(isTRUE(all.equal(def, cust)))
})

test_that("plot.interpret works on a data frame missing interpret class", {
  interpret(g) %>%
    dplyr::filter(coefficient > 0) %>%
    plot.interpret(print = FALSE) %>%
    expect_s3_class("gg")
})
