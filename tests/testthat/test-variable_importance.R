context("Checking variable importance")

data(mtcars)
mtcars$am <- factor(ifelse(mtcars$am, "Y", "N"))
set.seed(6529)
cl <- machine_learn(mtcars, outcome = am, tune = FALSE)
reg <- machine_learn(mtcars, outcome = mpg, models = "rf", tune = FALSE)
warns <- capture_warnings({
  cl_vi <- get_variable_importance(cl)
})
def <- get_variable_importance(reg)

test_that("order_models gets it right", {
  ci <- extract_model_info(cl)
  expect_equal(which(names(cl) == ci$best_model_name), unname(order_models(cl)[1]))
  expect_equal(ci$best_model_name, names(order_models(cl))[1])
  ri <- extract_model_info(reg)
  expect_equal(ri$best_model_name, names(order_models(reg))[1])
})

test_that("get_variable_importance returns a tibble", {
  expect_s3_class(cl_vi, "tbl_df")
  expect_s3_class(def, "tbl_df")
})

test_that("get_variable_importance dfs have class variable_importance", {
  expect_s3_class(cl_vi, "variable_importance")
  expect_s3_class(def, "variable_importance")
})

test_that("plot.variable_importance returns a ggplot", {
  expect_s3_class(plot(cl_vi, print = FALSE), "gg")
  expect_s3_class(plot(def, print = FALSE), "gg")
  expect_s3_class(plot(reg, caption = "none", title = "VI", font_size = 16, print = FALSE), "gg")
})

test_that("plot.variable_importance is registered generic", {
  expect_true("plot.variable_importance" %in% methods("plot"))
})

test_that("get_variable_importance doesn't use glmnet's", {
  gl <- machine_learn(mtcars, outcome = am, tune = FALSE, models = "glm")
  expect_error(get_variable_importance(gl), "Can't get")
  # glm is the best model in cl
  expect_false(attr(cl_vi, "model") == "glmnet")
})

test_that("get_variable_importance warns if providing model isn't best performer", {
  expect_true(stringr::str_detect(warns, "best performing"))
})

test_that("get_variable_importance respects top_n", {
  expect_equal(def, get_variable_importance(reg, top_n = 99))
  expect_equivalent(def[1, ], get_variable_importance(reg, top_n = 1))
})

test_that("get_variable_importance respects remove_zeros", {
  expect_equal(def, get_variable_importance(reg, remove_zeros = TRUE))
  expect_true(nrow(get_variable_importance(reg, remove_zeros = FALSE)) > nrow(def))
})

test_that("plot.variable_importance respects max_char", {
  expect_warning(
    expect_false(isTRUE(all.equal(plot(def, max_char = 3, print = FALSE),
                                plot(def, print = FALSE))))

  )
})
