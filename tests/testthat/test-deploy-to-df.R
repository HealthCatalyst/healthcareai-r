context("Checking deploy predictions to df")

csvfile <- system.file("extdata",
                       "HCRDiabetesClinical.csv",
                       package = "healthcareai")

df <- read.csv(file = csvfile,
               header = TRUE,
               na.strings =  c('NULL', 'NA', ""))

test_that("Lasso predicted val TO DF doesn't have NAs", {

  df$PatientID <- NULL
  p <- SupervisedModelDeploymentParams$new()
  p$type = 'classification'
  p$df = df
  p$grainCol = 'PatientEncounterID'
  p$testWindowCol = 'InTestWindowFLG'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  p$impute = TRUE
  p$debug = FALSE
  p$useSavedModel = FALSE
  p$cores = 1
  p$writeToDB = FALSE
  
  set.seed(43)
  
  capture.output(dL <- LassoDeployment$new(p))
  capture.output(dL$deploy())
  capture.output(df <- dL$getOutDf())
  
  expect_equal(as.numeric(sum(is.na(df))), 0)
})

test_that("rf predicted val TO DF doesn't have NAs", {
  
  df$PatientID <- NULL
  p <- SupervisedModelDeploymentParams$new()
  p$type = 'classification'
  p$df = df
  p$grainCol = 'PatientEncounterID'
  p$testWindowCol = 'InTestWindowFLG'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  p$impute = TRUE
  p$debug = FALSE
  p$useSavedModel = FALSE
  p$cores = 1
  p$writeToDB = FALSE
  
  set.seed(43)
  
  capture.output(dRF <- RandomForestDeployment$new(p))
  capture.output(dRF$deploy())
  capture.output(df <- dRF$getOutDf())
  
  expect_equal(as.numeric(sum(is.na(df))), 0)
})

test_that("LMM predicted val TO DF doesn't have NAs", {
  
  p <- SupervisedModelDeploymentParams$new()
  p$type = 'classification'
  p$df = df
  p$grainCol = 'PatientEncounterID'
  p$personCol = 'PatientID'
  p$testWindowCol = 'InTestWindowFLG'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  p$impute = TRUE
  p$debug = FALSE
  p$useSavedModel = FALSE
  p$cores = 1
  p$writeToDB = FALSE
  set.seed(43)
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p))
  capture.output(capture_warnings(dLMM$deploy()))
  capture.output(df <- dLMM$getOutDf())
  
  expect_equal(as.numeric(sum(is.na(df))), 0)
})