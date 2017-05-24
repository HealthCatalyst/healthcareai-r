context("Checking deploy predictions from csv to sqlite")

csvfile <- system.file("extdata",
                       "HCRDiabetesClinical.csv",
                       package = "healthcareai")

sqliteFile <- system.file("extdata",
                          "unit-test.sqlite",
                          package = "healthcareai")

df <- read.csv(file = csvfile,
               header = TRUE,
               na.strings = c("NULL", "NA", ""))

inTest <- df$InTestWindowFLG # save this for deploy

set.seed(43)
p <- SupervisedModelDevelopmentParams$new()
p$df = df
p$grainCol = 'PatientEncounterID'
p$impute = TRUE
p$debug = FALSE
p$cores = 1
p$tune = FALSE
p$numberOfTrees = 201

#### BEGIN TESTS ####

test_that("Lasso deploy classification pushes values to SQLite", {
  
  df$PatientID <- NULL #<- Note this happens affects all following tests
  df$InTestWindowFLG <- NULL
  
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  # Run Lasso
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  df$InTestWindowFLG <- inTest # put InTestWindowFLG back in
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'classification'
  p2$df = df
  p2$grainCol = 'PatientEncounterID'
  p2$testWindowCol = 'InTestWindowFLG'
  p2$predictedCol = 'ThirtyDayReadmitFLG'
  p2$impute = TRUE
  p2$sqlConn <- connection.string
  p2$destSchemaTable <- "dbo.HCRDeployClassificationBASE"
  
  capture.output(dL <- LassoDeployment$new(p2))
  capture.output(dL$deploy())
  capture.output(dfOut <- dL$getOutDf())
  expect_output(writeData(SQLiteFileName = sqliteFile,
                          df = dfOut,
                          tableName = 'HCRDeployClassificationBASE'),
                "13 rows were inserted into the SQLite")
})

test_that("Lasso deploy regression pushes values to SQLite", {

  df$InTestWindowFLG <- NULL
  
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'A1CNBR'
  
  # Run Lasso
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  df$InTestWindowFLG <- inTest # put InTestWindowFLG back in
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'regression'
  p2$df = df
  p2$grainCol = 'PatientEncounterID'
  p2$testWindowCol = 'InTestWindowFLG'
  p2$predictedCol = 'A1CNBR'
  p2$impute = TRUE
  p2$sqlConn <- connection.string
  p2$destSchemaTable <- "dbo.HCRDeployRegressionBASE"
  
  capture.output(dL <- LassoDeployment$new(p2))
  capture.output(dL$deploy())
  capture.output(dfOut <- dL$getOutDf())
  expect_output(writeData(SQLiteFileName = sqliteFile,
                          df = dfOut,
                          tableName = 'HCRDeployRegressionBASE'),
                "13 rows were inserted into the SQLite")
  
  closeAllConnections()
})