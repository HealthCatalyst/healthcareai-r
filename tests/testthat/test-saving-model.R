# These unit tests check that models can be saved under custom names
# 
# Tests include:
# - checking that a model trained without a custom name correctly uses the
#   the default model name
# - checking that a model trained with a custom name correctly uses the custom
#   name to generate save files
# - checking that models trained on the same data with the same parameters, but
#   with different model names make the same predictions

context("Checking saving models under custom names")

##############
# Test Setup #

library(healthcareai)

# default file names for random forest model and info
defaultFitObjFile <- "rmodel_probability_RF.rda"
defaultInfoFile <- "rmodel_info_RF.rda"

# get custom file names for random forest model and info
newName <- "non-deterministic-tree-ensemble"
customFitObjFile <- paste("rmodel_probability_", newName, "_RF.rda", sep = "")
customInfoFile <- paste("rmodel_info_", newName, "_RF.rda", sep = "")

# Remove model save files if they exist
for (file in c(defaultFitObjFile, 
               defaultInfoFile, 
               customFitObjFile, 
               customInfoFile)) {
  if (file.exists(file)) {
    file.remove(file)
  }
}

# Get data
csvfile <- system.file("extdata", 
                       "HCRDiabetesClinical.csv", 
                       package = "healthcareai")
df <- read.csv(file = csvfile, 
               header = TRUE, 
               na.strings = c("NULL", "NA", ""))
df$PatientID <- NULL # Only one ID column (ie, PatientEncounterID) is needed

# Save a dataframe for validation later on
dfDeploy <- df[951:1000,]

# Set up development parameters
p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "classification"
p$impute <- TRUE
p$grainCol <- "PatientEncounterID"
p$predictedCol <- "ThirtyDayReadmitFLG"
p$debug <- FALSE
p$cores <- 1

# Run RandomForest
set.seed(42)
capture.output(RandomForest <- RandomForestDevelopment$new(p))
capture.output(RandomForest$run())

# Set up deployment parameters
p2 <- SupervisedModelDeploymentParams$new()
p2$type <- "classification"
p2$df <- dfDeploy
p2$grainCol <- "PatientEncounterID"
p2$predictedCol <- "ThirtyDayReadmitFLG"
p2$impute <- TRUE
p2$debug <- FALSE
p2$cores <- 1

# Deploy model
capture.output(dL <- RandomForestDeployment$new(p2))
capture.output(dL$deploy())

# Save output for later
dfOutDefault <- dL$getOutDf()

#########
# TESTS #

test_that("Default model name is set correctly", {
  expect_true(file.exists(defaultFitObjFile))
  expect_true(file.exists(defaultInfoFile))
})

test_that("Model with custom name is saved correctly", {
  # Set up development parameters with custom name
  pCustom <- p
  pCustom$modelName <- newName
  
  # train model
  set.seed(42)
  capture.output(CustomForest <- RandomForestDevelopment$new(pCustom))
  capture.output(CustomForest$run())
  
  # Check that model name is recorded correctly
  expect_equal(CustomForest$params$modelName, newName)
  # check that saved files exist
  expect_true(file.exists(customFitObjFile))
  expect_true(file.exists(customInfoFile))
})


test_that("Model with custom name deploys correctly", {
  # remove default saved model to make sure the new model (customForest) is 
  # being used in deploy
  for (file in c(defaultFitObjFile, defaultInfoFile)) {
    if (file.exists(file)) {
      file.remove(file)
    }
  }
  
  # Set up deployment parameters with custom name
  pCustom2 <- p2
  pCustom2$modelName <- newName
  
  # Deploy model
  capture.output(dCustom <- RandomForestDeployment$new(p2))
  capture.output(dCustom$deploy())
  
  # Save output for later
  dfOutCustom <- dCustom$getOutDf()
  
  # Check that model name is recorded correctly
  expect_equal(dCustom$params$modelName, newName)
  # Check that the output of the models is the same
  expect_equal(dfOutDefault[1:3, 4:8], dfOutCustom[1:3, 4:8])
})
