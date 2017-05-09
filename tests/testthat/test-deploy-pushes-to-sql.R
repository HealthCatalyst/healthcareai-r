context("Checking deploy predictions from sql to sql")

test_that("Lasso deploy classification pushes values to SQL", {
  skip_on_travis()
  skip_on_cran()
  
  connection.string <- "
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
  ,[InTestWindowFLG]
  FROM [SAM].[dbo].[HCRDiabetesClinical]
  "
  df <- selectData(connection.string, query)
  
  df$PatientID <- NULL
  inTest <- df$InTestWindowFLG # save this for deploy
  df$InTestWindowFLG <- NULL
  
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  
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
  expect_output(dL$deploy(),
                "SQL Server insert was successful")
})

test_that("Lasso deploy regression pushes values to SQL", {
  skip_on_travis()
  skip_on_cran()
  
  connection.string <- "
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
  ,[InTestWindowFLG]
  FROM [SAM].[dbo].[HCRDiabetesClinical]
  "
  df <- selectData(connection.string, query)
  
  df$PatientID <- NULL
  inTest <- df$InTestWindowFLG # save this for deploy
  df$InTestWindowFLG <- NULL
  
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "regression"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "A1CNBR"

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
  expect_output(dL$deploy(),
                "SQL Server insert was successful")
})

test_that("rf deploy classification pushes values to SQL", {
  skip_on_travis()
  skip_on_cran()
  
  connection.string <- "
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
  ,[InTestWindowFLG]
  FROM [SAM].[dbo].[HCRDiabetesClinical]
  "
  df <- selectData(connection.string, query)
  
  df$PatientID <- NULL
  inTest <- df$InTestWindowFLG # save this for deploy
  df$InTestWindowFLG <- NULL
  
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  
  # Run RF
  dRF <- RandomForestDevelopment$new(p)
  capture.output(dRF$run())
  
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
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  expect_output(dRF$deploy(),
                "SQL Server insert was successful")
})

test_that("rf deploy regression pushes values to SQL", {
  skip_on_travis()
  skip_on_cran()
  
  connection.string <- "
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
  ,[InTestWindowFLG]
  FROM [SAM].[dbo].[HCRDiabetesClinical]
  "
  df <- selectData(connection.string, query)
  
  df$PatientID <- NULL
  inTest <- df$InTestWindowFLG # save this for deploy
  df$InTestWindowFLG <- NULL
  
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "regression"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "A1CNBR"
  
  # Run RF
  dRF <- RandomForestDevelopment$new(p)
  capture.output(dRF$run())
  
  df$InTestWindowFLG <- inTest # put InTestWindowFLG back in.
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'regression'
  p2$df = df
  p2$grainCol = 'PatientEncounterID'
  p2$testWindowCol = 'InTestWindowFLG'
  p2$predictedCol = 'A1CNBR'
  p2$impute = TRUE
  p2$sqlConn <- connection.string
  p2$destSchemaTable <- "dbo.HCRDeployRegressionBASE"
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  expect_output(dRF$deploy(),
                "SQL Server insert was successful")
})

test_that("LMM deploy classification pushes values to SQL", {
  skip_on_travis()
  skip_on_cran()
  
  connection.string <- "
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
  ,[InTestWindowFLG]
  FROM [SAM].[dbo].[HCRDiabetesClinical]
  "
  df <- selectData(connection.string, query)
  
  inTest <- df$InTestWindowFLG # save this for deploy
  df$InTestWindowFLG <- NULL
  
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$personCol <- "PatientID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  p$debug <- FALSE
  p$cores <- 1
  
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))
  
  df$InTestWindowFLG <- inTest # put InTestWindowFLG back in.
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- df
  p2$grainCol <- "PatientEncounterID"
  p$personCol <- "PatientID"
  p2$testWindowCol <- "InTestWindowFLG"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  p2$sqlConn <- connection.string
  p2$destSchemaTable <- "dbo.HCRDeployClassificationBASE"
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p2))
  expect_output(suppressWarnings(dLMM$deploy()),
                "SQL Server insert was successful")
})

test_that("LMM deploy regression pushes values to SQL", {
  skip_on_travis()
  skip_on_cran()
  
  connection.string <- "
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
  ,[InTestWindowFLG]
  FROM [SAM].[dbo].[HCRDiabetesClinical]
  "
  df <- selectData(connection.string, query)
  
  inTest <- df$InTestWindowFLG # save this for deploy
  df$InTestWindowFLG <- NULL
  
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "regression"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$personCol <- "PatientID"
  p$predictedCol <- "A1CNBR"
  
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))
  
  df$InTestWindowFLG <- inTest # put InTestWindowFLG back in.
            
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type = 'regression'
  p2$df = df
  p2$grainCol = 'PatientEncounterID'
  p$personCol <- "PatientID"
  p2$testWindowCol = 'InTestWindowFLG'
  p2$predictedCol = 'A1CNBR'
  p2$impute = TRUE
  p2$sqlConn <- connection.string
  p2$destSchemaTable <- "dbo.HCRDeployRegressionBASE"
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p2))
  expect_output(suppressWarnings(dLMM$deploy()),
                "SQL Server insert was successful")
})