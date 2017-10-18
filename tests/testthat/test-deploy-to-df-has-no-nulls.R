context("Checking deploy predictions from csv to df")


  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")

  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))

  dfDeploy <- df[951:1000,]

  #### BEGIN TESTS ####

test_that("LMM predicted df doesn't have NAs", {
  
  # Create LMM model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$personCol = 'PatientID'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))

  # Deploy lmm model
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
  
  expect_equal(as.numeric(sum(is.na(dfRes))), 0)
  closeAllConnections()
})
test_that("Lasso predicted df doesn't have NAs", {

  df$PatientID <- NULL
  dfDeploy <- df[951:1000,]
  
  # Create lasso model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  # Deploy lasso model
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
  
  expect_equal(as.numeric(sum(is.na(dfRes))), 0)
  closeAllConnections()
})

test_that("rf predicted df doesn't have NAs", {
  
  # Create rf model
  set.seed(43)
  p <- initializeParamsForTesting(df)
  p$type = 'classification'
  p$predictedCol = 'ThirtyDayReadmitFLG'
  
  RandomForest <- RandomForestDevelopment$new(p)
  capture.output(RandomForest$run())
  
  # Deploy rf model
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
  
  expect_equal(as.numeric(sum(is.na(dfRes))), 0)
  closeAllConnections()
})