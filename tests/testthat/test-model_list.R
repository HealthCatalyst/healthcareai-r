context("Checking model_list constructors")

# Setup
set.seed(8301)
d <- data.frame(x = rnorm(10), y = sample(c("a", "b"), 10, TRUE))
rf <- ranger::ranger(y ~ x, d)
kn <- kknn::kknn(y ~ x, d[1:8, ], d[9:10, ])

test_that("model_list fails if type is unsupported", {
  expect_error(model_list(type = "what am i even?"))
  # Update once supported:
  expect_error(model_list(type = "unsupervised"))
  expect_error(model_list(type = "multiclass"))
})

test_that("model_list succeeds without model input", {
  empty_reg <- model_list("regression")
  expect_s3_class(empty_reg, "regression_list")
  expect_s3_class(empty_reg, "model_list")
  empty_class <- model_list("classification")
  expect_s3_class(empty_class, "classification_list")
  expect_s3_class(empty_class, "model_list")
})

test_that("as.model_list fails if type is unsupported and warns if input isn't model", {   # nolint
  expect_warning(as.model_list(1:5, type = "regression"))
  expect_error(as.model_list(type = "what am i even?"))
})

test_that("as.model_list succeeds with empty input", {
  as.model_list(type = "regression")
  as.model_list(type = "classification")
})

test_that("as.model_list succeeds with one or more models as input", {
  # Note that we don't check the outcome variable against model type here
  as.model_list(rf, type = "classification")
  as.model_list(rf, kn, type = "regression")
  as.model_list(listed_models = list(rf, kn), type = "classification")
  as.model_list(listed_models = list(rf), type = "regression")
})

test_that("as.model_list preserves model names", {
  m_names <- c("rando", "knn")
  m_list <- structure(list(rf, kn), names = m_names)
  expect_equal(
    names(as.model_list(listed_models = m_list, type = "classification")),
    m_names
  )
  expect_equal(
    names(as.model_list(rf, kn, type = "classification")),
    c("rf", "kn")
  )
})
