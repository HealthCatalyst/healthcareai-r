context("Checking deploy supervised model prediction values to DF")

  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")

  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))

  dfDeploy <- df[951:1000,]

  #### BEGIN TESTS ####

test_that("mixed model classification predicted val is the same each time", {
  
  # Create LMM model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$personCol = 'PatientID'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))
  
  # Depoy LMM model

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
  capture.output(suppressWarnings(dLMM$deploy()))
  capture.output(dfRes <- dLMM$getOutDf())
  
  expect_true(abs(dfRes$PredictedProbNBR[1] - 0.3353753) < 1.0e-2)
  expect_true(abs(dfRes$PredictedProbNBR[10] - 0.9881623) < 1.0e-2)
  closeAllConnections()
})
  
test_that("mixed model regression predicted val is the same each time", {
    
  # Create LMM model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$personCol = 'PatientID'
  p$predictedCol = 'SystolicBPNBR'
    
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))
    
  # Depoy LMM model
    
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "regression"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientEncounterID"
  p2$personCol <- "PatientID"
  p2$predictedCol <- "SystolicBPNBR"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
    
  capture.output(dLMM <- LinearMixedModelDeployment$new(p2))
  capture.output(suppressWarnings(dLMM$deploy()))
  capture.output(dfRes <- dLMM$getOutDf())
    
  expect_true(abs(dfRes$PredictedValueNBR[1] - 128.0015) < 1.0e-2)
  expect_true(abs(dfRes$PredictedValueNBR[10] - 163.5445) < 1.0e-2)
  closeAllConnections()
})

test_that("rf classification predicted val (w/out mtry tuning) is the same each 
          time", {

  df$PatientID <- NULL # affects all future tests
  dfDeploy <- df[951:1000,]
  
  # Create rf model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  capture.output(RandomForest <- RandomForestDevelopment$new(p))
  capture.output(RandomForest$run())
  
  # Depoy rf model

  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  capture.output(dRF$deploy())
  capture.output(dfRes <- dRF$getOutDf())
  #here, tolerances need to be high in order for the Travis build to work on mac
  expect_true(abs(dfRes$PredictedProbNBR[1] - 0.08362355) < .1)
  expect_true(abs(dfRes$PredictedProbNBR[10] - 0.9438937) < .1)
  closeAllConnections()
})

test_that("rf classification predicted val (w/ mtry tuning) is the same each 
          time", {

  df$PatientID <- NULL # affects all future tests
  dfDeploy <- df[951:1000,]
  
  # Create rf model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  p$tune <- TRUE
  
  capture.output(RandomForest <- RandomForestDevelopment$new(p))
  capture.output(RandomForest$run())
  
  # Depoy rf model
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  capture.output(dRF$deploy())
  capture.output(dfRes <- dRF$getOutDf())
  
  # for some reason, this tolerance needs to be very lax...
  expect_true(abs(dfRes$PredictedProbNBR[1] - 0.09225302) < .1)
  expect_true(abs(dfRes$PredictedProbNBR[10] - 0.9169635) < .1)
  closeAllConnections()
})

test_that("rf regression predicted val (w/out mtry tuning) is same each time", {
  
  df$PatientID <- NULL
  dfDeploy <- df[951:1000,]
  
  # Create rf model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'SystolicBPNBR'
  
  capture.output(RandomForest <- RandomForestDevelopment$new(p))
  capture.output(RandomForest$run())
  
  # Depoy rf model
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "regression"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "SystolicBPNBR"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  capture.output(dRF$deploy())
  capture.output(dfRes <- dRF$getOutDf())
  
  #here, tolerances need to be high in order for the Travis build to work on mac
  expect_true(abs(dfRes$PredictedValueNBR[7] - 157.333) < .2)
  expect_true(abs(dfRes$PredictedValueNBR[19] - 148.4243) < .2)
  closeAllConnections()
})


test_that("rf regression predicted val (w/mtry tuning) is same each time", {
  
  df$PatientID <- NULL
  dfDeploy <- df[951:1000,]
  
  # Create rf model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'SystolicBPNBR'
  p$tune <- TRUE
  
  capture.output(RandomForest <- RandomForestDevelopment$new(p))
  capture.output(RandomForest$run())
  
  # Depoy rf model
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "regression"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- 'SystolicBPNBR'
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  capture.output(dRF$deploy())
  capture.output(dfRes <- dRF$getOutDf())
  
  # for some reason, this tolerance needs to be very lax...
  expect_true(abs(dfRes$PredictedValueNBR[15] - 136.2595) < .5)
  expect_true(abs(dfRes$PredictedValueNBR[23] - 162.4714) < .5)
  closeAllConnections()
})

test_that("lasso classification predicted val is the same each time", {
  
  df$PatientID <- NULL # affects all future tests
  dfDeploy <- df[951:1000,]
  
  # Create lasso model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  # Depoy lasso model
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dL <- LassoDeployment$new(p2))
  capture.output(dL$deploy())
  capture.output(dfRes <- dL$getOutDf())
  
  expect_true(abs(dfRes$PredictedProbNBR[1] - 0.1566765) < 1.0e-2)
  expect_true(abs(dfRes$PredictedProbNBR[10] - 0.2221281) < 1.0e-2)
  closeAllConnections()
})

test_that("lasso regression predicted val is the same each time", {
  
  df$PatientID <- NULL
  dfDeploy <- df[951:1000,]
  
  # Create lasso model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'regression'
  p$predictedCol = 'SystolicBPNBR'
  
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  # Depoy lasso model
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "regression"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "SystolicBPNBR"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dL <- LassoDeployment$new(p2))
  capture.output(dL$deploy())
  capture.output(dfRes <- dL$getOutDf())
  
  expect_true(abs(dfRes$PredictedValueNBR[50] - 149.6703) < 1.0e-2)
  expect_true(abs(dfRes$PredictedValueNBR[27] - 149.6703) < 1.0e-2)
  closeAllConnections()
})
