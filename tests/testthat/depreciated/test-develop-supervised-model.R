context("Checking develop supervised model with impute true")

csvfile <- system.file("extdata",
                       "HCRDiabetesClinical.csv",
                       package = "healthcareai")

df <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings =  c('NULL', 'NA', ""))

set.seed(43)
p <- SupervisedModelDevelopmentParams$new()
p$df = df
p$grainCol = 'PatientEncounterID'
p$impute = TRUE
p$debug = FALSE
p$cores = 1
p$tune = FALSE
p$trees = 201

###########
# Common settings

test_that("Error is thrown when type != regression or classification or multiclass", {
  p$type = 'a'
  expect_error(LassoDevelopment$new(p), 'Your type must be regression, classification, or multiclass')
})

test_that("Error is thrown when predicted column isn't binary and type is classification", {
  p$type = 'classification'
  p$predictedCol = 'A1CNBR'
  expect_error(LassoDevelopment$new(p), 'Dependent variable must be binary for classification')
})

test_that("Error is thrown when predicted column is binary and type is regression", {
  p$type = 'regression'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  expect_error(LassoDevelopment$new(p), 'Dependent variable cannot be binary for regression')
})

#############
#LinearMixedModel (has to go before rf/lasso, bc the latter delete PersonID)

test_that("AUC_lmm is the same each time the test is run", {

  skip_on_cran()
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$personCol = 'PatientID'
  p$predictedCol = 'ThirtyDayReadmitFLG'

  capture.output(lmm <- LinearMixedModelDevelopment$new(p))
  capture.output(expect_warning(lmm$run()))

  expect_true(as.numeric(lmm$getAUROC()) - 0.9222222 < 1.0e-6)

})

test_that("rmse_lmm is the same each time the test is run", {

  skip_on_cran()
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$personCol = 'PatientID'
  p$predictedCol = 'A1CNBR'

  capture.output(lmm <- LinearMixedModelDevelopment$new(p))
  capture.output(lmm$run())

  expect_true(as.numeric(lmm$getRMSE()) - 1.013385 < 1.0e-6)

})

test_that("mae_lmm is the same each time the test is run", {

  skip_on_cran()
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$personCol = 'PatientID'
  p$predictedCol = 'A1CNBR'

  capture.output(lmm <- LinearMixedModelDevelopment$new(p))
  capture.output(lmm$run())

  expect_true(as.numeric(lmm$getMAE()) - 0.71855611 < 1.0e-6)

})

###########
#Lasso

test_that("AUC_lasso is the same each time the test is run", {
  df$PatientID <- NULL #<- Note this happens affects all following tests

  p <- initializeParamsForTesting(df)
  p$df = df
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'

  capture.output(grLasso <- LassoDevelopment$new(p))
  capture.output(grLasso$run())

  expect_true(as.numeric(grLasso$getAUROC()) - 0.7353293 < 1.0e-6)
})

test_that("rmse_lasso is the same each time the test is run non-factor column", {

  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'A1CNBR'

  capture.output(grLasso <- LassoDevelopment$new(p))
  capture.output(grLasso$run())

  expect_true(as.numeric(grLasso$getRMSE()) - 1.14930401 < 1.0e-6)

})

test_that("mae_lasso is the same each time the test is run non-factor column", {

  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'A1CNBR'

  capture.output(grLasso <- LassoDevelopment$new(p))
  capture.output(grLasso$run())

  expect_true(as.numeric(grLasso$getMAE()) - 0.95895099 < 1.0e-6)

})

#############
#RandomForest

test_that("AUC_rf is the same each time the test is run", {
  
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'

  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())

  expect_true(as.numeric(rf$getAUROC()) - 0.9393 < 1.0e-2)

})

test_that("rmse_rf is the same each time the test is run non-factor column", {

  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'A1CNBR'

  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())

  expect_true(as.numeric(rf$getRMSE()) - 1.048997 < 1.0e-6) #5.75427780855753)

})

test_that("mae_rf is the same each time the test is run non-factor column", {

  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'A1CNBR'

  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())

  expect_true(as.numeric(rf$getMAE()) - 0.8370312 < 1.0e-6) #2.9240442062118)

})

test_that("dev grainCol not in dev df gives proper error", {
  
  p$type <- "classification"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  p$grainCol <- "Winnie the Pooh"
  
  expect_error(RandomForestDevelopment$new(p), "Your specified grainCol is not")
  
})

test_that("dev predictedCol not in dev df gives proper error", {
  
  p$grainCol <- 'PatientEncounterID'
  p$type <- "classification"
  p$predictedCol <- "fleshEatingDandruff"
  
  expect_error(RandomForestDevelopment$new(p), "Your specified predictedCol is")
})
