context("Checking deploy supervised model")

csvfile <- system.file("extdata",
                       "HCRDiabetesClinical.csv",
                       package = "healthcareai")

df <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings =  c('NULL', 'NA', ""))

connection.string <- 'driver={SQL Server};
                      server=localhost;
                      database=SAM;
                      trusted_connection=true'

df$PatientID <- NULL

test_that("rf predicted val (with spec mtry) is the same each time", {
  skip_on_travis()
  skip_on_cran()
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

  expect_equal(as.numeric(dRF$getPredictedValsForUnitTest()), 0.03765893)
})

test_that("lasso predicted val (with spec mtry) is the same each time", {
  skip_on_travis()
  skip_on_cran()
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
  skip_on_travis()
  skip_on_cran()
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

  expect_equal(as.numeric(dRF$getPredictedValsForUnitTest()), 0.03470742)
})

test_that("lasso predicted val (w/out spec mtry) is the same each time", {
  skip_on_travis()
  skip_on_cran()
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



