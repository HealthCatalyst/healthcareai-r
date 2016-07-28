context("Checking develop supervised model")

csvfile <- system.file("extdata", "HREmployeeDev.csv", package = "HCRTools")
totaldf <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings = 'NULL')

test_that("Error is thrown when type != regression or classification", {
  expect_error(DevelopSupervisedModel$new(type = 'a',
                                          df = totaldf,
                                          predicted.col = 'SalariedFlag',
                                          impute = FALSE),
               'Your type must be regression or classification')
})

test_that("Error is thrown when predicted column isn't binary and type is classification", {
  expect_error(DevelopSupervisedModel$new(type = 'classification',
                                          df = totaldf,
                                          predicted.col = 'OrganizationLevel',
                                          impute = FALSE),
               'Dependent variable must be binary for classification')
})

test_that("Error is thrown when predicted column is binary and type is regression", {
  expect_error(DevelopSupervisedModel$new(type = 'regression',
                                          df = totaldf,
                                          predicted.col = 'SalariedFlag',
                                          impute = FALSE),
               'Dependent variable cannot be binary for regression')
})

test_that("AUC_lasso is the same each time the test is run", {
  set.seed(43)
  o <- DevelopSupervisedModel$new(type = 'classification',
                                  df = totaldf,
                                  predicted.col = 'SalariedFlag',
                                  impute = FALSE)
  capture.output(o$grlasso(var.imp = TRUE,
            cores = 1,
            debug = FALSE))

  expect_equal(as.numeric(o$AUC_lasso),0.800236406619385)
})

test_that("rmse_lasso is the same each time the test is run non-factor column", {
  set.seed(43)
  o <- DevelopSupervisedModel$new(type = 'regression',
                                  df = totaldf,
                                  predicted.col = 'SickLeaveHours',
                                  impute = FALSE)
  capture.output(o$grlasso(var.imp = TRUE,
                           cores = 1,
                           debug = TRUE))

  expect_equal(as.numeric(o$rmse_lasso),5.35177750023664)
})

test_that("mae_lasso is the same each time the test is run non-factor column", {
  set.seed(43)
  o <- DevelopSupervisedModel$new(type = 'regression',
                                  df = totaldf,
                                  predicted.col = 'SickLeaveHours',
                                  impute = FALSE)
  capture.output(o$grlasso(var.imp = TRUE,
                           cores = 1,
                           debug = TRUE))

  expect_equal(as.numeric(o$mae_lasso),2.6402310211195)
})

test_that("AUC_rf is the same each time the test is run", {
  set.seed(43)
  o <- DevelopSupervisedModel$new(type = 'classification',
                                  df = totaldf,
                                  predicted.col = 'SalariedFlag',
                                  impute = FALSE)
  capture.output(o$randForest(cores = 1,
                              debug = FALSE))

  expect_equal(as.numeric(o$AUC_rf),0.914893617021277)
})

test_that("rmse_rf is the same each time the test is run non-factor column", {
  set.seed(43)
  o <- DevelopSupervisedModel$new(type = 'regression',
                                  df = totaldf,
                                  predicted.col = 'SickLeaveHours',
                                  impute = FALSE)
  capture.output(o$randForest(cores = 1,
                              debug = FALSE))

  expect_equal(as.numeric(o$rmse_rf),5.75480614)
})

test_that("mae_rf is the same each time the test is run non-factor column", {
  set.seed(43)
  o <- DevelopSupervisedModel$new(type = 'regression',
                                  df = totaldf,
                                  predicted.col = 'SickLeaveHours',
                                  impute = FALSE)
  capture.output(o$randForest(cores = 1,
                              debug = FALSE))

  expect_equal(as.numeric(o$mae_rf),2.91958186)
})
