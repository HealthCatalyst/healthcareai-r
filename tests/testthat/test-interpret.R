context("Checking interpret")

set.seed(271)
m <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = diabetes)
multi <- machine_learn(na.omit(pima_diabetes[1:200, ]), patient_id, outcome = weight_class,
                       tune = FALSE, models = "glm")
g <- m["glmnet"]

test_that("test errors", {
  expect_error(interpret(list()), "x must be a model_list")
})

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

test_that("interpret top_n works right", {
  t5 <- interpret(g, top_n = 5)
  expect_equal(5, nrow(t5))
  expect_equivalent(as.data.frame(t5), as.data.frame(interpret(g))[1:5, ])
  expect_equal(interpret(g), interpret(g, top_n = 99))
})

test_that("Coefficient signs make sense WRT positive class", {
  # For pima_diabetes, normal weight class should be negative
  ip <- interpret(g)
  expect_true(ip$coefficient[ip$variable == "weight_class_normal (vs. obese)"] < 0)
  n <- 100
  # x1_y should be positive
  d <- tibble::tibble(
    y = c(rep("N", n / 2), rep("Y", n / 2)),
    x1 = c(rep("n", n * .4), rep("y", n * .5), rep("n", n * .1)),
    x2 = 0
  )
  m <-
    d %>%
    prep_data(outcome = y, remove_near_zero_variance = FALSE) %>%
    flash_models(outcome = y, models = "glm")
  i <- interpret(m)
  expect_true(i$coefficient[i$variable == "x1_y (vs. n)"] > 0)
  # Declaring positive class. Here x1_y should be negative
  m <-
    d %>%
    prep_data(outcome = y, remove_near_zero_variance = FALSE) %>%
    flash_models(outcome = y, models = "glm", positive_class = "N")
  i <- interpret(m)
  expect_true(i$coefficient[i$variable == "x1_y (vs. n)"] < 0)
  # With 0/1 outcome
  m <-
    dplyr::mutate(d, y = dplyr::case_when(y == "Y" ~ 1L, y == "N" ~ 0L)) %>%
    prep_data(outcome = y, remove_near_zero_variance = FALSE) %>%
    flash_models(outcome = y, models = "glm")
  i <- interpret(m)
  expect_true(i$coefficient[i$variable == "x1_y (vs. n)"] > 0)
})

test_that("alpha gets attached to interpret objects even if glm isn't best", {
  # Note: On my OSX machine RF outperforms glmnet which correctly triggers a
  # warning on `interpret(m)`. On Windows no warning is given, perhaps
  # because glmnet is the best performing model???
  suppressWarnings(i3 <- interpret(m))
  expect_equal(attr(i3, "alpha"), attr(interpret(g), "alpha"))
})

context("Checking plot.interpret")  # ------------------------------------------

test_that("plot.interpret is registered generic", {
  expect_true("plot.interpret" %in% methods("plot"))
})

test_that("test errors", {
  test <- list()
  attr(test, "class") <- "interpret"
  expect_error(plot(test), "x must be a data frame from interpret")
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
  cust <- plot(ig, caption = "my custom caption", print = FALSE)
  trunc <- plot(ig, max_char = 10, print = FALSE)
  pseudo_trunc <- plot(ig, max_char = 999, print = FALSE)
  expect_false(isTRUE(all.equal(def, keep_int)))
  expect_false(isTRUE(all.equal(def, cust)))
  expect_false(isTRUE(all.equal(def, trunc)))
})

test_that("plot.interpret works on a data frame missing interpret class", {
  interpret(g) %>%
    dplyr::filter(coefficient > 0) %>%
    plot.interpret(print = FALSE) %>%
    expect_s3_class("gg")
})

context("Checking print.interpret")  # ------------------------------------------

test_that("print.interpret is registered generic", {
  expect_true("print.interpret" %in% methods(class = "interpret"))
})

test_that("test setting reference level/ print.reference_level", {
  n <- 100
  d <- tibble::tibble(
    y = c(rep("N", n / 2), rep("Y", n / 2)),
    x1 = c(rep("n", n * .4), rep("y", n * .5), rep("n", n * .1)),
    x2 = 0
  )
  m <-
    d %>%
    prep_data(outcome = y, remove_near_zero_variance = FALSE) %>%
    flash_models(outcome = y, models = "glm")
  i <- interpret(m, remove_zeros = FALSE)

  output <- capture_output(print(i))
  expect_true(length(gregexpr("Reference Levels:\n", output)[[1]]) == 1)
  expect_false(grepl("All `y` estimates are relative to `N`", output))
  expect_true(grepl("All `x1` estimates are relative to `n`", output))
})

context("Checking add_refs") # -------------------------------------------------

test_that("test add_refs normal functionality", {
  dat <- tibble(
    variable = c("weight_class_normal", "skinfold"),
    coefficient = c(1, .500)
  )
  m <- machine_learn(pima_diabetes[0:50, ], outcome = diabetes, models = "glm",
                     tune = FALSE)
  dummy_step_object <- get_recipe_step(m, "step_dummy_hcai")
  actual <- add_refs(dat, dummy_step_object)
  expect_true("weight_class_normal (vs. obese)" %in% actual$variable)
  expect_true("skinfold" %in% actual$variable)
})

test_that("multiclass isn't supported", {
  expect_error(interpret(multi), "multiclass")
})
