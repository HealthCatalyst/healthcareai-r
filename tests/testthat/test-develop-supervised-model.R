context("Checking develop supervised model")

csvfile <- system.file("extdata", "HREmployeeDev.csv", package = "HCRTools")
totaldf <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings = 'NULL')

p <- SupervisedModelParameters$new()
p$df = totaldf
p$type = 'classification'
p$impute = TRUE
p$grainCol = ''
p$predictedCol = 'SalariedFlag'
p$debug = FALSE
p$varImp = TRUE
p$printResults = TRUE
p$cores = 1
p$tune = FALSE
p$numberOfTrees = 201


###########
# Common settings

test_that("Error is thrown when type != regression or classification", {
  p$type = 'a'
  expect_error(Lasso$new(p), 'Your type must be regression or classification')
})

test_that("Error is thrown when predicted column isn't binary and type is classification", {
  p$type = 'classification'
  p$predictedCol = 'OrganizationLevel'
  expect_error(Lasso$new(p), 'Dependent variable must be binary for classification')
})

test_that("Error is thrown when predicted column is binary and type is regression", {
  p$type = 'regression'
  p$predictedCol = 'SalariedFlag'
  expect_error(Lasso$new(p), 'Dependent variable cannot be binary for regression')
})

###########
#Lasso

test_that("AUC_lasso is the same each time the test is run", {
  set.seed(43)
  p$type = 'classification'
  p$predictedCol = 'SalariedFlag'

  capture.output(grlasso <- Lasso$new(p))
  capture.output(grlasso$run())

  expect_true(as.numeric(grlasso$getAUC()) - 0.8263889 < 1.0e-6) #0.800236406619385)
})

test_that("rmse_lasso is the same each time the test is run non-factor column", {
  set.seed(43)
  p$type = 'regression'
  p$predictedCol = 'SickLeaveHours'

  capture.output(grlasso <- Lasso$new(p))
  capture.output(grlasso$run())

  expect_true(as.numeric(grlasso$getRMSE()) - 5.2771911 < 1.0e-6) #5.35177750023664)

})

test_that("mae_lasso is the same each time the test is run non-factor column", {
  set.seed(43)
  p$type = 'regression'
  p$predictedCol = 'SickLeaveHours'

  capture.output(grlasso <- Lasso$new(p))
  capture.output(grlasso$run())

  expect_true(as.numeric(grlasso$getMAE()) - 2.615982 < 1.0e-6)# 2.6402310211195)

})


#############
#RandomForest

test_that("AUC_rf is the same each time the test is run", {
  set.seed(43)
  p$type = 'classification'
  p$predictedCol = 'SalariedFlag'
  p$impute = FALSE

  capture.output(rf <- RandomForest$new(p))
  capture.output(rf$run())

  expect_true(as.numeric(rf$getAUC()) - 0.9148936 < 1.0e-6) #0.914893617021277)

})

test_that("rmse_rf is the same each time the test is run non-factor column", {
  set.seed(43)
  p$type = 'regression'
  p$predictedCol = 'SickLeaveHours'
  p$impute = FALSE

  capture.output(rf <- RandomForest$new(p))
  capture.output(rf$run())

  expect_true(as.numeric(rf$getRMSE()) - 5.754806 < 1.0e-6) #5.75427780855753)

})

test_that("mae_rf is the same each time the test is run non-factor column", {
  set.seed(43)
  p$type = 'regression'
  p$predictedCol = 'SickLeaveHours'
  p$impute = FALSE

  capture.output(rf <- RandomForest$new(p))
  capture.output(rf$run())

  expect_true(as.numeric(rf$getMAE()) - 2.919582 < 1.0e-6) #2.9240442062118)

})
