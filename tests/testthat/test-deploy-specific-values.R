context("Checking deploy supervised model")

test_that("rf predicted val (w/out mtry tuning) is the same each time", {
  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")
  
  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))
  
  df$PatientID <- NULL
  inTest <- df$InTestWindowFLG # save this for later.
  df$InTestWindowFLG <- NULL
  
  # Create rf model
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  
  RandomForest <- RandomForestDevelopment$new(p)
  capture.output(RandomForest$run())
  
  # Depoy rf model
  df$InTestWindowFLG <- inTest
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- df
  p2$testWindowCol <- "InTestWindowFLG"
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  p2$writeToDB <- FALSE
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  capture.output(dRF$deploy())
  capture.output(dfRes <- dRF$getOutDf())
  
  expect_true(as.numeric(dfRes$PredictedProbNBR[1]) - 0.03285276 < 1.0e-2)
})

test_that("rf predicted val (w/ mtry tuning) is the same each time", {
  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")
  
  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))
  
  df$PatientID <- NULL
  inTest <- df$InTestWindowFLG # save this for later.
  df$InTestWindowFLG <- NULL
  
  # Create rf model
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  p$tune <- TRUE
  
  RandomForest <- RandomForestDevelopment$new(p)
  capture.output(RandomForest$run())
  
  # Depoy rf model
  df$InTestWindowFLG <- inTest
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- df
  p2$testWindowCol <- "InTestWindowFLG"
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  p2$writeToDB <- FALSE
  
  capture.output(dRF <- RandomForestDeployment$new(p2))
  capture.output(dRF$deploy())
  capture.output(dfRes <- dRF$getOutDf())
  expect_true(as.numeric(dfRes$PredictedProbNBR[1]) - 0.04258364 < 1.0e-2)
})

test_that("lasso predicted val is the same each time", {
  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")
  
  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))
  
  df$PatientID <- NULL
  inTest <- df$InTestWindowFLG # save this for later.
  df$InTestWindowFLG <- NULL
  
  # Create lasso model
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  
  lasso <- LassoDevelopment$new(p)
  capture.output(lasso$run())
  
  # Depoy lasso model
  df$InTestWindowFLG <- inTest
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- df
  p2$testWindowCol <- "InTestWindowFLG"
  p2$grainCol <- "PatientEncounterID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  p2$writeToDB <- FALSE
  
  capture.output(dL <- LassoDeployment$new(p2))
  capture.output(dL$deploy())
  capture.output(dfRes <- dL$getOutDf())
  
  expect_true(dfRes$PredictedProbNBR[1] - 0.1052794 < 1.0e-2)
})

test_that("mixed model predicted val is the same each time", {
  csvfile <- system.file("extdata",
                         "HCRDiabetesClinical.csv",
                         package = "healthcareai")
  
  df <- read.csv(file = csvfile,
                 header = TRUE,
                 na.strings =  c('NULL', 'NA', ""))
  
  inTest <- df$InTestWindowFLG # save this for later.
  df$InTestWindowFLG <- NULL
  
  # Create LMM model
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- df
  p$type <- "classification"
  p$impute <- TRUE
  p$grainCol <- "PatientEncounterID"
  p$personCol <- "PatientID"
  p$predictedCol <- "ThirtyDayReadmitFLG"
  
  LinearMixedModel <- LinearMixedModelDevelopment$new(p)
  capture.output(suppressWarnings(LinearMixedModel$run()))
  
  # Depoy LMM model
  df$InTestWindowFLG <- inTest
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- df
  p2$testWindowCol <- "InTestWindowFLG"
  p2$grainCol <- "PatientEncounterID"
  p2$personCol <- "PatientID"
  p2$predictedCol <- "ThirtyDayReadmitFLG"
  p2$impute <- TRUE
  p2$debug <- FALSE
  p2$cores <- 1
  p2$writeToDB <- FALSE
  
  capture.output(dLMM <- LinearMixedModelDeployment$new(p2))
  capture.output(suppressWarnings(dLMM$deploy()))
  capture.output(dfRes <- dLMM$getOutDf())
  
  expect_true(dfRes$PredictedProbNBR[1] - 6.975151e-05 < 1.0e-2)
})