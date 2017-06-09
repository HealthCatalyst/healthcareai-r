context("Checking xgboost deployment")

library(healthcareai)

# 1. Load data. Categorical columns should be characters.
csvfile <- system.file("extdata", 
                       "dermatology_multiclass_data.csv", 
                       package = "healthcareai")

# Replace csvfile with 'path/file'
df <- read.csv(file = csvfile, 
               header = TRUE, 
               stringsAsFactors = FALSE,
               na.strings = c("NULL", "NA", "", "?"))

str(df) # check the types of columns
dfDevelop <- df[1:346,] # use most of data to train and evalute the model.
dfDeploy <- df[347:366,] # reserve 20 rows for deploy step.

# 2. Develop and save model
set.seed(42)
p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "multiclass"
p$impute <- TRUE
p$grainCol <- "PatientID"
p$predictedCol <- "target"
p$debug <- FALSE
p$cores <- 1
# xgb_params must be a list with all of these things in it. 
# if you would like to tweak parameters, go for it! 
# Leave objective and eval_metric as they are.
p$xgb_params <- list("objective" = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "max_depth" = 6, # max depth of each learner
                     "eta" = 0.1, # learning rate
                     "silent" = 0, # verbose output when set to 1
                     "nthread" = 2) # number of processors to use
# Run model
xNew <- capture.output(boost <- XGBoostDevelopment$new(p))
xRun <- capture.output(boost$run())

## 3. Load saved model (automatic) and use DEPLOY to generate predictions. 
p2 <- SupervisedModelDeploymentParams$new()
p2$type <- "multiclass"
p2$df <- dfDeploy
p2$grainCol <- "PatientID"
p2$predictedCol <- "target"
p2$impute <- TRUE
p2$debug <- FALSE

# Deploy model to make new predictions
xNew <- capture.output(boostD <- XGBoostDeployment$new(p2))
xDeploy <- capture.output(boostD$deploy())

# Get output dataframe for csv or SQL
xDf <- capture.output(outDf <- boostD$getOutDf())

###########
# Multiclass

test_that("Grain is correctly attached", {
  expect_true(outDf$PatientID[1] == 347)
  expect_true(outDf$PatientID[4] == 350)
})

test_that("Probabilities are correctly sorted", {
  expect_true(round(outDf$PredictedProb1[1],5) == 0.91198)
  expect_true(round(outDf$PredictedProb2[4],5) == 0.08339)
  expect_true(round(outDf$PredictedProb3[7],5) == 0.00312)
})

test_that("Top categories are correctly parsed", {
  expect_true(outDf$PredictedClass1[2] == 'six')
  expect_true(outDf$PredictedClass1[5] == 'one')
  expect_true(outDf$PredictedClass1[9] == 'five')
})
