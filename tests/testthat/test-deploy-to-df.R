context("Checking deploy predictions to df")

csvfile <- system.file("extdata",
                       "HCRDiabetesClinical.csv",
                       package = "healthcareai")

df <- read.csv(file = csvfile,
               header = TRUE,
               na.strings =  c('NULL', 'NA', ""))

df$PatientID <- NULL

test_that("rf predicted val TO DF doesn't have NAs", {
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
  p$destSchemaTable = 'dbo.HCRDeployClassificationBASE'

  set.seed(43)
  
  capture.output(dRF <- RandomForestDeployment$new(p))
  capture.output(dRF$deploy())
  capture.output(df <- dRF$getOutDf())
  
  expect_equal(as.numeric(sum(is.na(df))), 0)
})