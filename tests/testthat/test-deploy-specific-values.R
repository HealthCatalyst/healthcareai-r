context("Checking deploy supervised model prediction values to DF")

  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")

  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))

  dfDevelop <- df[df$InTestWindowFLG=='N',!(colnames(df)=='InTestWindowFLG')]
  dfDeployClassification <- df[df$InTestWindowFLG=='Y',!(colnames(df)=='InTestWindowFLG')]

  dfDeployRegression <- dfDeployClassification
  dfDeployClassification$ThirtyDayReadmitFLG <- NULL 
  dfDeployRegression$A1CNBR <- NULL

  #### BEGIN TESTS ####

test_that("mixed model predicted val is the same each time", {
  
  # Create LMM model
  set.seed(43)
  p <- initializeParamsForTesting(dfDevelop)
  p$type = 'classification'
  p$personCol = 'PatientID'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))
  
  # Depoy LMM model

  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeployClassification
  p2$grainCol <- "PatientEncounterID"
  p2$personCol <- "PatientID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p2))
  capture.output(suppressWarnings(dLMM$deploy()))
  capture.output(dfRes <- dLMM$getOutDf())
  
  expect_true(abs(dfRes$PredictedProbNBR[1] - 6.975151e-05) < 1.0e-2)
  closeAllConnections()
})

test_that("rf predicted val (w/out mtry tuning) is the same each time", {

  df$PatientID <- NULL # affects all future tests
  
  # Create rf model
  set.seed(43)
  p <- initializeParamsForTesting(dfDevelop)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  capture.output(RandomForest <- RandomForestDevelopment$new(p))
  capture.output(RandomForest$run())
  
  # Depoy rf model

  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeployClassification
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  capture.output(dRF$deploy())
  capture.output(dfRes <- dRF$getOutDf())
  
  expect_true(abs(dfRes$PredictedProbNBR[1] - 0.03285276) < 1.0e-2)
  closeAllConnections()
})

test_that("rf predicted val (w/ mtry tuning) is the same each time", {
  
  # Create rf model
  set.seed(43)
  p <- initializeParamsForTesting(dfDevelop)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  p$tune <- TRUE
  
  capture.output(RandomForest <- RandomForestDevelopment$new(p))
  capture.output(RandomForest$run())
  
  # Depoy rf model
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeployClassification
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  capture.output(dRF$deploy())
  capture.output(dfRes <- dRF$getOutDf())

  expect_true(abs(dfRes$PredictedProbNBR[1] - 0.04258364) < 1.0e-2)
  closeAllConnections()
})

test_that("lasso predicted val is the same each time", {
  
  # Create lasso model
  set.seed(43)
  p <- initializeParamsForTesting(dfDevelop)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  # Depoy lasso model
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeployClassification
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  
  capture.output(dL <- LassoDeployment$new(p2))
  capture.output(dL$deploy())
  capture.output(dfRes <- dL$getOutDf())
  
  expect_true(abs(dfRes$PredictedProbNBR[1] - 0.1052794) < 1.0e-2)
  closeAllConnections()
})
