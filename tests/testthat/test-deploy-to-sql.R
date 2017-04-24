context("Checking deploy predictions from csv to sql")

csvfile <- system.file("extdata",
                       "HCRDiabetesClinical.csv",
                       package = "healthcareai")

df <- read.csv(file = csvfile,
               header = TRUE,
               na.strings =  c('NULL', 'NA', ""))

connection.string <- "
  driver={SQL Server};
  server=localhost;
  database=SAM;
  trusted_connection=true
"

test_that("lasso classification doesn't return any errors when pushing 
          predictions to SQL", {
  skip_on_travis()
  skip_on_cran()
  
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
  p$sqlConn <- connection.string
  p$destSchemaTable <- "dbo.HCRDeployClassificationBASE"
  
  set.seed(43)
  
  capture.output(dL <- LassoDeployment$new(p))
  expect_output(dL$deploy(),
                "SQL Server insert was successful")
})

test_that("lasso regression doesn't return any errors when pushing predictions 
          to SQL", {
  skip_on_travis()
  skip_on_cran()
  
  df$PatientID <- NULL
  p <- SupervisedModelDeploymentParams$new()
  p$type = 'regression'
  p$df = df
  p$grainCol = 'PatientEncounterID'
  p$testWindowCol = 'InTestWindowFLG'
  p$predictedCol = 'A1CNBR'
  p$impute = TRUE
  p$debug = FALSE
  p$useSavedModel = FALSE
  p$cores = 1
  p$sqlConn <- connection.string
  p$destSchemaTable <- "dbo.HCRDeployRegressionBASE"
  
  set.seed(43)
  
  capture.output(dL <- LassoDeployment$new(p))
  expect_output(dL$deploy(),
                "SQL Server insert was successful")
})

test_that("rf classification doesn't return any errors when pushing predictions 
          to SQL", {
  skip_on_travis()
  skip_on_cran()
  
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
  p$sqlConn <- connection.string
  p$destSchemaTable <- "dbo.HCRDeployClassificationBASE"
  
  set.seed(43)
  
  capture.output(dRF <- RandomForestDeployment$new(p))
  expect_output(dRF$deploy(),
                "SQL Server insert was successful")
})

test_that("rf regression doesn't return any errors when pushing predictions to 
          SQL", {
  skip_on_travis()
  skip_on_cran()
  
  df$PatientID <- NULL
  p <- SupervisedModelDeploymentParams$new()
  p$type = 'regression'
  p$df = df
  p$grainCol = 'PatientEncounterID'
  p$testWindowCol = 'InTestWindowFLG'
  p$predictedCol = 'A1CNBR'
  p$impute = TRUE
  p$debug = FALSE
  p$useSavedModel = FALSE
  p$cores = 1
  p$sqlConn <- connection.string
  p$destSchemaTable <- "dbo.HCRDeployRegressionBASE"
  
  set.seed(43)
  
  capture.output(dRF <- RandomForestDeployment$new(p))
  expect_output(dRF$deploy(),
                "SQL Server insert was successful")
})

test_that("LMM classification doesn't return any errors when pushing predictions 
          to SQL", {
  skip_on_travis()
  skip_on_cran()
  
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
  p$sqlConn <- connection.string
  p$destSchemaTable <- "dbo.HCRDeployClassificationBASE"
  
  set.seed(43)
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p))
  expect_output(suppressWarnings(dLMM$deploy()),
                "SQL Server insert was successful")
})

test_that("LMM regression doesn't return any errors when pushing predictions 
          to SQL", {
  skip_on_travis()
  skip_on_cran()
            
  p <- SupervisedModelDeploymentParams$new()
  p$type = 'regression'
  p$df = df
  p$grainCol = 'PatientEncounterID'
  p$personCol = 'PatientID'
  p$testWindowCol = 'InTestWindowFLG'
  p$predictedCol = 'A1CNBR'
  p$impute = TRUE
  p$debug = FALSE
  p$useSavedModel = FALSE
  p$cores = 1
  p$sqlConn <- connection.string
  p$destSchemaTable <- "dbo.HCRDeployRegressionBASE"
  
  set.seed(43)
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p))
  expect_output(suppressWarnings(dLMM$deploy()),
                "SQL Server insert was successful")
})