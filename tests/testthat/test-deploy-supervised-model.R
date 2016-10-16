context("Checking deploy supervised model")

csvfile <- system.file("extdata", "HCRDiabetesClinical.csv", package = "HCRTools")
df <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings =  c('NULL', 'NA', ""))

connection.string <- 'driver={SQL Server};
                      server=localhost;
                      database=SAM;
                      trusted_connection=true'

df$PatientID <- NULL

test_that("rf predicted val (with spec mtry) is the same each time", {

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
  p$sqlConn = connection.string
  p$destSchemaTable = 'dbo.HCRDeployClassificationBASE'
  p$rfmtry = 3

  set.seed(43)

  capture.output(dRF <- RandomForestDeployment$new(p))
  capture.output(dRF$deploy())

  expect_equal(as.numeric(dRF$getPredictedValsForUnitTest()), 0.03000869)
})

test_that("lasso predicted val (with spec mtry) is the same each time", {

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
  p$sqlConn = connection.string
  p$destSchemaTable = 'dbo.HCRDeployClassificationBASE'
  p$rfmtry = 3

  set.seed(43)

  capture.output(dL <- LassoDeployment$new(p))
  capture.output(dL$deploy())

  expect_equal(as.numeric(dL$getPredictedValsForUnitTest()), 0.02694817)
})

test_that("rf predicted val (w/out spec mtry) is the same each time", {

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
  p$sqlConn = connection.string
  p$destSchemaTable = 'dbo.HCRDeployClassificationBASE'

  set.seed(43)

  capture.output(dRF <- RandomForestDeployment$new(p))
  capture.output(dRF$deploy())

  expect_equal(as.numeric(dRF$getPredictedValsForUnitTest()), 0.03352681)
})

test_that("lasso predicted val (w/out spec mtry) is the same each time", {

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
  p$sqlConn = connection.string
  p$destSchemaTable = 'dbo.HCRDeployClassificationBASE'

  set.seed(43)

  capture.output(dL <- LassoDeployment$new(p))
  capture.output(dL$deploy())

  expect_equal(as.numeric(dL$getPredictedValsForUnitTest()), 0.02694817)
})



