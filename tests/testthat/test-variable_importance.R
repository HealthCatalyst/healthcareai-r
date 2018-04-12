context("Checking variable importance")

data(mtcars)
mtcars$am <- factor(ifelse(mtcars$am, "Y", "N"))
cl <- machine_learn(mtcars, outcome = am, tune = FALSE)
reg <- machine_learn(mtcars, outcome = mpg, models = "rf", tune = FALSE)

test_that("rank_models gets it right", {
  ci <- extract_model_info(cl)
  expect_equal(which(names(cl) == ci$best_model_name), which(rank_models(cl) == 1))
  ri <- extract_model_info(reg)
  expect_equal(which(names(reg) == ri$best_model_name), which(rank_models(reg) == 1))
})

test_that("get_variable_importance returns a tibble", {
  expect_s3_class(get_variable_importance(cl), "tbl_df")
  expect_s3_class(get_variable_importance(reg), "tbl_df")
})

test_that("plot_variable_importance returns a ggplot", {
  expect_s3_class(plot_variable_importance(cl, print = FALSE), "gg")
  expect_s3_class(plot_variable_importance(reg, print = FALSE), "gg")
  expect_s3_class(plot_variable_importance(get_variable_importance(cl), print = FALSE), "gg")
  expect_s3_class(plot_variable_importance(get_variable_importance(reg), print = FALSE), "gg")
  expect_s3_class(plot_variable_importance(reg, caption = "none", title = "VI", font_size = 16, print = FALSE), "gg")
})
