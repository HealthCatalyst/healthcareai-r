context("Checking deploy supervised model")

test_that("rf predicted val (w/out mtry tuning) is the same each time", {
  skip_on_travis()
  skip_on_cran()

  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")
  
  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))
  
  df$PatientID <- NULL
    
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  p$debug <- FALSE
  p$cores <- 1
  
  dRF <- RandomForestDevelopment$new(p)
  capture.output(dRF$run())
  
  # Read from saved model and create predictions
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- df
  p2$testWindowCol <- "InTestWindowFLG"
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  # TODO: remove saved model flag. 
  p2$useSavedModel <- TRUE # this is always TRUE now.
  p2$impute <- TRUE
  p2$writeToDB <- FALSE
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  suppressWarnings(capture.output(dRF$deploy()))
  capture.output(dfRes <- dRF$getOutDf())
  
  expect_true(as.numeric(dfRes$PredictedProbNBR[1]) - 0.03285276 < 1.0e-2)
})

test_that("rf predicted val (w/ mtry tuning) is the same each time", {
  skip_on_travis()
  skip_on_cran()
  
  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")
  
  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))
  
  df$PatientID <- NULL
  
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  p$impute <- TRUE
  p$tune <- TRUE
  
  dRF <- RandomForestDevelopment$new(p)
  capture.output(dRF$run())
  
  # Read from saved model and create predictions
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- df
  p2$testWindowCol <- "InTestWindowFLG"
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  # TODO: remove saved model flag. 
  p2$useSavedModel <- TRUE # this is always TRUE now.
  p2$impute <- TRUE
  p2$writeToDB <- FALSE
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  suppressWarnings(capture.output(dRF$deploy()))
  capture.output(dfRes <- dRF$getOutDf())
  expect_true(as.numeric(dfRes$PredictedProbNBR[1]) - 0.04258364 < 1.0e-2)
})

test_that("lasso predicted val is the same each time", {
  skip_on_travis()
  skip_on_cran()
  
  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")
  
  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))
  
  df$PatientID <- NULL
  
  set.seed(43)
  # Create saved Lasso model
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  p$debug <- FALSE
  p$cores <- 1
  
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  # Read from saved model and create predictions
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- df
  p2$testWindowCol <- "InTestWindowFLG"
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  # TODO: remove saved model flag. 
  p2$useSavedModel <- TRUE # this is always TRUE now.
  p2$impute <- TRUE
  p2$debug <- TRUE
  p2$cores <- 1
  p2$writeToDB <- FALSE
  
  capture.output(dL <- LassoDeployment$new(p2))
  suppressWarnings(capture.output(dL$deploy()))
  capture.output(dfRes <- dL$getOutDf())
  
  expect_true(dfRes$PredictedProbNBR[1] - 0.1052794 < 1.0e-4)
})