# Setup ------------------------------------------------------------------------
data(mtcars)
mtcars$am <- as.factor(c("automatic", "manual")[mtcars$am + 1])
rf <- caret::train(x = dplyr::select(mtcars, -am),
                   y = mtcars$am,
                   method = "ranger",
                   trControl = trainControl(method = "none"),
                   tuneGrid = data.frame(mtry = 3,
                                         splitrule = "gini",
                                         min.node.size = 2)
)
kn <- caret::train(x = dplyr::select(mtcars, -am),
                   y = mtcars$am,
                   method = "kknn",
                   trControl = trainControl(method = "none"),
                   tuneGrid = data.frame(kmax = 3,
                                         distance = 1,
                                         kernel = "rectangular")
)
r_models <- tune(mtcars, mpg)
c_models <- tune(mtcars, am)

context("Checking model_list constructors") # ----------------------------------

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

test_that("as.model_list fails if type is unsupported", {
  expect_error(as.model_list(type = "what am i even?"))
})

test_that("as.model_list warns if input isn't a model", {
  expect_warning(as.model_list(1:5, type = "regression"))
})

test_that("as.model_list succeeds with empty input", {
  expect_s3_class(as.model_list(type = "regression"), "regression_list")
  expect_s3_class(as.model_list(type = "classification"), "classification_list")
})

test_that("as.model_list succeeds with one or more models as input", {
  # Note that we don't check the outcome variable against model type here
  expect_s3_class(as.model_list(rf, type = "classification"), "model_list")
  expect_s3_class(as.model_list(rf, kn, type = "regression"), "model_list")
  expect_s3_class(
    as.model_list(listed_models = list(rf, kn), type = "classification"),
    "model_list"
  )
  expect_s3_class(
    as.model_list(listed_models = list(rf), type = "regression"),
    "model_list"
  )
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

context("Checking model_list plotters") # --------------------------------------

test_that("plot.regression_list works", {
  expect_equal(class(plot(r_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot.regression_list(r_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_error(plot.regression_list(c_models, print = FALSE),
               regexp = "classification")
  r2 <- tune(mtcars, mpg, metric = "Rsquared")
  expect_s3_class(plot(r2, print = FALSE), "gg")
})

# test_that("plot.classification_list works", {
#   expect_error(plot.classification_list(r_models, print = FALSE),
#                regexp = "regression")
# })
