context("Checking model tuning")

# Setup ------------------------------------------------------------------------
test_df <- tibble::tibble(
  x1 = rep(letters[1:2], each = 10),
  x2 = rnorm(20),
  x3 = c(1:19, 25)
)

test_that("tune doesn't error on simple regression or classification use", {
  expect_error(tune(test_df, x3, "regression", "lasso", 1, 1), NA)
  expect_error(tune(test_df, x1, "classification", "RF", 1, 1), NA)
})

test_that("tune returns a model_list", {
  expect_s3_class(tune(test_df, x3, "regression", "lasso", 1, 1), "model_list")
})
