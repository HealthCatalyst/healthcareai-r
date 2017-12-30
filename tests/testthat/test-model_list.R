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

test_that("model lists have target attribute", {
  expect_equal(attr(model_list(type = "classification"), "target"), ".outcome")
  expect_equal(attr(r_models, "target"), "mpg")
  expect_equal(attr(c_models, "target"), "am")
})

test_that("as.model_list fails if type is unsupported", {
  expect_error(as.model_list(type = "what am i even?"))
})

test_that("as.model_list warns if input isn't a caret model", {
  expect_error(as.model_list(1:5, type = "regression"))
  expect_error(as.model_list(ranger::ranger(mpg ~ ., mtcars)))
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

context("Checking model_list generics") # --------------------------------------

test_that("plot.model_list works on regression_list", {
  expect_equal(class(plot(r_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot.model_list(r_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_error(plot.model_list(ranger::ranger(mpg ~ ., mtcars), print = FALSE),
               regexp = "model_list")
  r2 <- tune(mtcars, mpg, models = "rf", metric = "Rsquared")
  expect_s3_class(plot(r2, print = FALSE), "gg")
})

test_that("plot.model_list works on classification_list", {
  expect_equal(class(plot(c_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_equal(class(plot.model_list(c_models, print = FALSE)),
               c("gg", "ggplot"))
  expect_error(plot.model_list(ranger::ranger(am ~ ., mtcars), print = FALSE),
               regexp = "model_list")
})

test_that("print.model_list works", {
  rprint <- capture_output(r_models, TRUE)
  expect_true(nchar(rprint) > 0)
  expect_true(grepl("regression", rprint, ignore.case = TRUE))
  cprint <- capture_output(c_models, TRUE)
  expect_true(nchar(cprint) > 0)
  expect_true(grepl("classification", cprint, ignore.case = TRUE))
})

test_that("summary.model_list works", {
  rsumout <- capture_output(rsum <- summary(r_models), TRUE)
  expect_true(nchar(rsumout) > 0)
  expect_true(grepl("hyperparameters", rsumout, ignore.case = TRUE))
  expect_true(is.list(rsum))
  expect_true(rlang::is_named(rsum))

  csumout <- capture_output(csum <- summary(c_models), TRUE)
  expect_true(nchar(csumout) > 0)
  expect_true(grepl("hyperparameters", csumout, ignore.case = TRUE))
  expect_true(is.list(csum))
  expect_true(rlang::is_named(csum))
})
