context("Checking deploy predictions from sql server to sql server")

  connectionString <- "
  driver={SQL Server};
  server=localhost;
  database=SAM;
  trusted_connection=true
  "
  
  query <- "
  SELECT
  [PatientID]
  ,[PatientEncounterID] --Only need one ID column for random forest/lasso
  ,[SystolicBPNBR]
  ,[LDLNBR]
  ,[A1CNBR]
  ,[GenderFLG]
  ,[ThirtyDayReadmitFLG]
  FROM [SAM].[dbo].[HCRDiabetesClinical]
  "
  df <- selectData(connectionString, query)
  
  dfDeploy <- df[951:1000,]
  
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

 test_that("LMM deploy classification pushes values to SQL", {

  skip_on_travis()
  skip_on_cran()

  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$personCol = 'PatientID'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientEncounterID"
  p2$personCol <- "PatientID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p2))
  out <- capture.output(suppressWarnings(dLMM$deploy()))
  capture.output(dfOut <- dLMM$getOutDf())
  expect_output(writeData(MSSQLConnectionString = connectionString,
                          df = dfOut,
                          tableName = 'HCRDeployClassificationBASE'),
                "13 rows were inserted into the SQL Server table HCRDeployClassificationBASE")
  closeAllConnections()
})

test_that("LMM deploy regression pushes values to SQL", {

  skip_on_travis()
  skip_on_cran()
  
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$personCol = 'PatientID'
  p$predictedCol = 'A1CNBR'
  
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))
            
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'regression'
  p2$df = dfDeploy
  p2$grainCol = 'PatientEncounterID'
  p2$personCol <- "PatientID"
  p2$predictedCol = 'A1CNBR'
  p2$impute = TRUE
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p2))
  out <- capture.output(suppressWarnings(dLMM$deploy()))
  capture.output(dfOut <- dLMM$getOutDf())
  expect_output(writeData(MSSQLConnectionString = connectionString,
                          df = dfOut,
                          tableName = 'HCRDeployRegressionBASE'),
                "13 rows were inserted into the SQL Server table HCRDeployRegressionBASE")
  closeAllConnections()
}) 

test_that("Lasso deploy classification pushes values to SQL Server", {

  skip_on_travis()
  skip_on_cran()

  df$PatientID <- NULL #<- Note this happens affects all following tests
  
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  # Run Lasso
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'classification'
  p2$df = dfDeploy
  p2$grainCol = 'PatientEncounterID'
  p2$predictedCol = 'ThirtyDayReadmitFLG'
  p2$impute = TRUE
  
  capture.output(dL <- LassoDeployment$new(p2))
  capture.output(dL$deploy())
  capture.output(dfOut <- dL$getOutDf())
  expect_output(writeData(MSSQLConnectionString = connectionString,
                          df = dfOut,
                          tableName = 'HCRDeployClassificationBASE'),
                "13 rows were inserted into the SQL Server table HCRDeployClassificationBASE")
})

test_that("Lasso deploy regression pushes values to SQL Server", {

  skip_on_travis()
  skip_on_cran()
  
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'A1CNBR'

  # Run Lasso
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'regression'
  p2$df = dfDeploy
  p2$grainCol = 'PatientEncounterID'
  p2$predictedCol = 'A1CNBR'
  p2$impute = TRUE
  
  capture.output(dL <- LassoDeployment$new(p2))
  out <- capture.output(dL$deploy())
  capture.output(dfOut <- dL$getOutDf())
  expect_output(writeData(MSSQLConnectionString = connectionString,
                          df = dfOut,
                          tableName = 'HCRDeployRegressionBASE'),
                "13 rows were inserted into the SQL Server table HCRDeployRegressionBASE")
})

test_that("rf deploy classification pushes values to SQL Server", {

  skip_on_travis()
  skip_on_cran()
  
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  # Run RF
  dRF <- RandomForestDevelopment$new(p)
  capture.output(dRF$run())
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'classification'
  p2$df = dfDeploy
  p2$grainCol = 'PatientEncounterID'
  p2$predictedCol = 'ThirtyDayReadmitFLG'
  p2$impute = TRUE
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  out <- capture.output(dRF$deploy())
  capture.output(dfOut <- dRF$getOutDf())
  expect_output(writeData(MSSQLConnectionString = connectionString,
                          df = dfOut,
                          tableName = 'HCRDeployClassificationBASE'),
                "13 rows were inserted into the SQL Server table HCRDeployClassificationBASE")
  closeAllConnections()
})

test_that("rf deploy regression pushes values to SQL Server", {

  skip_on_travis()
  skip_on_cran()
  
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'A1CNBR'

  # Run RF
  dRF <- RandomForestDevelopment$new(p)
  capture.output(dRF$run())
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'regression'
  p2$df = dfDeploy
  p2$grainCol = 'PatientEncounterID'
  p2$predictedCol = 'A1CNBR'
  p2$impute = TRUE
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  out <- capture.output(dRF$deploy())
  capture.output(dfOut <- dRF$getOutDf())
  expect_output(writeData(MSSQLConnectionString = connectionString,
                          df = dfOut,
                          tableName = 'HCRDeployRegressionBASE'),
                "13 rows were inserted into the SQL Server table HCRDeployRegressionBASE")
  closeAllConnections()
})
