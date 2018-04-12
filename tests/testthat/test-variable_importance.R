context("Checking variable importance")

data(mtcars)
mtcars$am <- factor(ifelse(mtcars$am, "Y", "N"))
cl <- machine_learn(mtcars, outcome = am)
reg <- machine_learn(mtcars, outcome = mpg)

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
