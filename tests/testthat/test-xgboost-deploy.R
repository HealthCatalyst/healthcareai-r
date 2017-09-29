# The tests in this file all deal with XGBoost deployment, but are separated 
# into two groups
#
# The tests listed under multiclass tests check that the results of XGBoost
# are consistent: running the same code on the same data should yield the same
# results every time. Specifically, the tests check that
#  - the grain column ids are correct
#  - the predicted classes and their order are correct
#  - the probabilities of the predicted classes are correct
#
# The factor level tests are similar to those found in test-factor-level
# -coercion and include:
# 1. Check that single row predictions work
#    - build single row deploy data frame and check that deploy outputs a
#      prediction
#    - note that this implicitly tests that predictions can be made when there
#      are missing factors in one or more columns (for a single row, all but 
#      one level of each factor will be missing)
# 2. Check that predictions are independent
#    - build a single row deploy data set and a data setcontaining all of the 
#      training data, all of which share the same first row
#    - check that the predictions are the same on both data sets for the shared
#      row (i.e. the values taken in other rows of the data have no effect on
#      the prediction)
# 3. Check new factor levels are dealt with correctly
#    - build data sets with new factor levels not seen in develop
#    - check that the correct warning is raised
#    - check that these new levels are treated as NA by comparing the
#      prediction to a prediction made on data with NAs instead of the new 
#      factor levels
# A new test, specific to XGBoost is
# 4. Check that XGBoost behaves the same regardless of whether strings are
#    treated as factors or characters
#    - build a dataset with stringsAsFactors = TRUE
#    - build the same dataset with stringsAsFactors = FALSE
#    - check that the outputs are the same for models trained on both sets

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
# Text of warning that we know will trigger and that we want to suppress
warningText = paste("Each of the following categorical variable levels occurs ",
                    "3 times or fewer:\n-  x6 : Class1\n-  x27 : Class1\n-  ",
                    "x33 : Class1\nConsider grouping",
                    sep = "")
# Run model
xNew <- capture.output(ignoreSpecWarn(code = boost <- XGBoostDevelopment$new(p),
                                      wRegexps = warningText))
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

##################
# Multiclass Tests

test_that("Grain is correctly attached", {
  expect_true(outDf$PatientID[1] == 347)
  expect_true(outDf$PatientID[4] == 350)
})

test_that("Probabilities are correctly sorted", {
  expect_true(round(outDf$PredictedProb1[1],5) == 0.91198)
  expect_true(round(outDf$PredictedProb2[4],5) == 0.01804)
  expect_true(round(outDf$PredictedProb3[7],5) == 0.00312)
})

test_that("Top categories are correctly parsed", {
  expect_true(outDf$PredictedClass1[2] == 'six')
  expect_true(outDf$PredictedClass1[5] == 'one')
  expect_true(outDf$PredictedClass1[9] == 'five')
})

###########################
# Factor Level Test Setup #

# This function sets up the supervised model development parameters for the 
# factor level tests.  In particular, a toy multiclass dataset is generated
# with the option of treating strings as factors or characters (determined by
# the asFactor parameter).
getToyXGBDevelopParams = function(asFactor) {
  set.seed(112358)
  # Build toy data set for multiclass classification with two categorical
  # variables, shape and color
  n = 200
  colors = c("green", "red", "yellow")
  shapes = c("curved", "round", "straight")
  d <- data.frame(id = 1:n,
                  color = sample(colors, n, replace = TRUE),
                  shape = sample(shapes, n, replace = TRUE))
  
  # Add response variable.
  d["object"] <- ifelse(d$color == "yellow", 
                        ifelse(d$shape == "curved", 
                               "banana", 
                               ifelse(d$shape == "round", 
                                      "lemon", 
                                      "squash")), 
                        ifelse(d$shape == "curved", 
                               "chili",
                               ifelse(d$shape == "round",
                                      "apple",
                                      ifelse(d$color == "green", 
                                             "cucumber", 
                                             "straight-red-thingy"))))
  # Add noise by scrambling 5% of the response variables
  noise <- sample(1:n, size = n/20, replace = F)
  classes <- c("banana", "lemon", "squash", "chili", "apple", "cucumber", 
               "straight-red-thingy")
  d$object[noise] <- sample(classes, size = n/20, replace = T)
  # Convert strings to factor/character
  if (asFactor) {
    d$color <- as.factor(d$color)
    d$shape <- as.factor(d$shape)
    d$object <- as.factor(d$object)
  } else {
    d$color <- as.character(d$color)
    d$shape <- as.character(d$shape)
    d$object <- as.character(d$object)
  }
  
  # develop xgboost model
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- d
  p$type <- "multiclass"
  p$impute <- TRUE
  p$grainCol <- "id"
  p$predictedCol <- "object"
  p$debug <- FALSE
  p$cores <- 1
  p$xgb_params <- list("objective" = "multi:softprob",
                       "eval_metric" = "mlogloss",
                       "max_depth" = 6, # max depth of each learner
                       "eta" = 0.1, # learning rate
                       "silent" = 0, # verbose output when set to 1
                       "nthread" = 2) # number of processors to use
  return(p)
}

####################
# Factor Level Tests

test_that("Single row predictions work for xgboost", {
  p <- getToyXGBDevelopParams(asFactor = T)
  capture.output(boost <- XGBoostDevelopment$new(p))
  capture.output(boost$run())
  
  dfDeploy <- data.frame(id = 1776,
                         color = "yellow",
                         shape = "curved")
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "multiclass"
  p2$df <- dfDeploy
  p2$grainCol <- "id"
  p2$predictedCol <- "object"
  p2$impute <- TRUE
  p2$debug <- F
  # deploy model
  xNew <- capture.output(boostD <- XGBoostDeployment$new(p2))
  xDeploy <- capture.output(boostD$deploy())
  xDf <- capture.output(outDf <- boostD$getOutDf())
  
  # Check the id, top prediction probability, and top prediction class
  expect_equal(outDf$id[1], 1776)
  expect_equal(round(outDf$PredictedProb1[1],5), 0.90045)
  expect_equal(outDf$PredictedClass1[1], "banana")
})

test_that("XGBoost predictions are independent of each other", {
  p <- getToyXGBDevelopParams(asFactor = T)
  capture.output(boost <- XGBoostDevelopment$new(p))
  capture.output(boost$run())
  
  dfDeploy <- data.frame(id = 1776,
                          color = "yellow",
                          shape = "curved")
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "multiclass"
  p2$df <- dfDeploy
  p2$grainCol <- "id"
  p2$predictedCol <- "object"
  p2$impute <- TRUE
  p2$debug <- F
  
  # deploy model on single row
  xNew <- capture.output(boostD <- XGBoostDeployment$new(p2))
  xDeploy <- capture.output(boostD$deploy())
  xDf <- capture.output(outDf <- boostD$getOutDf())
  
  # add training data to deploy dataframe
  dfFull <- rbind(dfDeploy, p$df[c("id", "color", "shape")])
  pFull <- p2
  pFull$df <- dfFull
  # deploy model on same row plus full training data
  xNewF <- capture.output(boostDFull <- XGBoostDeployment$new(pFull))
  xDeployF <- capture.output(boostDFull$deploy())
  xDfF <- capture.output(outDfFull <- boostDFull$getOutDf())
  
  # Check that the predictions match
  expect_equal(outDf[1, 4:10], outDfFull[1, 4:10])
})

test_that("Extra factors are imputed correctly for XGBoost", {
  p <- getToyXGBDevelopParams(asFactor = T)
  capture.output(boost <- XGBoostDevelopment$new(p))
  capture.output(boost$run())
  
  # Deploy set contains new factor levels. These should be set to NA and
  # imputed
  dfDeploy <- data.frame(id = c(1789, 1790, 1791),
                         color = c("golden", NA, "yellow"),
                         shape = c("unbending", NA, "straight"))
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "multiclass"
  p2$df <- dfDeploy
  p2$grainCol <- "id"
  p2$predictedCol <- "object"
  p2$impute <- TRUE
  p2$debug <- F
  # deploy model
  xNew <- capture.output(boostD <- XGBoostDeployment$new(p2))
  # Check that warning is triggered
  expect_warning(xDeploy <- capture.output(boostD$deploy()),
                 paste("New categorical variable levels were found:\n",
                       " -  color : golden\n",
                       " -  shape : unbending\n",
                       "These values have been set to NA.",
                       sep = ""))
  
  # Get output dataframe for csv or SQL
  xDf <- capture.output(outDf <- boostD$getOutDf())
  
  # Check the predictions of the first 2 rows match
  expect_equal(outDf[1, 5], outDf[2, 5])
  expect_equal(outDf[1, 6], outDf[2, 6])
  expect_equal(outDf[1, 7], outDf[2, 7])
})

test_that("Factors and characters give the same results", {
  # train model on dataset with factors
  p <- getToyXGBDevelopParams(asFactor = T)
  capture.output(boost <- XGBoostDevelopment$new(p))
  capture.output(boost$run())
  
  # deploy set also uses factors
  dfDeployFact <- data.frame(id = c(1776, 1767),
                             color = c("yellow", "green"),
                             shape = c("curved", "round"),
                             stringsAsFactors = T)
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "multiclass"
  p2$df <- dfDeployFact
  p2$grainCol <- "id"
  p2$predictedCol <- "object"
  p2$impute <- TRUE
  p2$debug <- F
  
  # deploy model and get output dataframe for factors
  xNewFact <- capture.output(boostDFact <- XGBoostDeployment$new(p2))
  xDeployFact <- capture.output(boostDFact$deploy())
  xDfFact <- capture.output(outDfFact <- boostDFact$getOutDf())
  
  # develop model on same dataset, but with characters instead of factors
  p <- getToyXGBDevelopParams(asFactor = F)
  capture.output(boost <- XGBoostDevelopment$new(p))
  capture.output(boost$run())
  
  # deploy set also uses characters
  dfDeployChar <- data.frame(id = c(1776, 1767),
                             color = c("yellow", "green"),
                             shape = c("curved", "round"),
                             stringsAsFactors = F)
  p2$df <- dfDeployChar
  
  # deploy model and get output dataframe for characters
  xNewChar <- capture.output(boostDChar <- XGBoostDeployment$new(p2))
  xDeployChar <- capture.output(boostDChar$deploy())
  xDfChar <- capture.output(outDfChar <- boostDChar$getOutDf())
  
  # Check that we get the same results when using factors/characters
  expect_equal(outDfFact[, 4:10], outDfChar[, 4:10])
})


