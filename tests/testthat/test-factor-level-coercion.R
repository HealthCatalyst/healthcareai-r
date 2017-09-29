# The purpose of these unit tests it to check that factor levels from develop 
# are being correctly applied to the deploy data, allowing for single row (and
# other small deploy data set) predictions.
#
# Several tests are performed for each of lasso and random forest (both 
# classification and regression) and LMM (just classification).  These are
#
# 1. Check that single row predictions work
#    - build single row deploy data frame and check that deploy outputs a
#      prediction
#    - note that this implicitly tests that predictions can be made when there
#      are missing factors in one or more columns (for a single row, all but 
#      one level of each factor will be missing)
#    - check that single row predictions work even if there are NAs present
#
# 2. Check that predictions are independent
#    - build a single row deploy data set, a 2 row data set, and a data set
#      containing all of the training data, all of which share the same first
#      row
#    - check that the predictions are the same on all three data sets for the
#      shared row (i.e. the values taken in other rows of the data have no 
#      effect on the prediction)
#
# 3. Check new factor levels are dealt with correctly
#    - build data sets with new factor levels not seen in develop
#    - check that the correct warning is raised
#    - check that these new levels are treated as NA by comparing the
#      prediction to a prediction made on data with NAs instead of the new 
#      factor levels
#    - in one test, a single new factor level is introduced in a single column
#    - in a separate test, multiple new factor levels are introduced in two 
#      separate columns
#
# Additional tests:
# 1. check that Y/N in classification model is consistent with the data
#    - make sure model isn't treating Y as N and vice versa
#    - test with characters as well as factors (including intentionally setting
#      N and Y in the wrong order)


context("Checking factor level coercion is working")

# set seed for reproducibility
set.seed(7)

# build hot dog set
n = 300
df <- data.frame(id = 1:n,
                 vendorID = sample(1:9, size = n, replace = T),
                 length = rnorm(n, mean = 7, sd = 2),
                 diameter = rnorm(n, mean = 2, sd = 0.5), 
                 heat = sample(c("Cold", "Hot"), size = n, replace = T),
                 condiment = sample(c("Ketchup", "Mustard", "Wasabi", "Syrup"), 
                                    size = n, replace = T)
                 )

# give hotdog likeliness score
df['hotDogScore'] <- df['length'] - 2*df['diameter'] - 1
df$hotDogScore[df['heat'] == "Hot"]  = df$hotDogScore[df['heat'] == "Hot"] + 1
df$hotDogScore[df['heat'] == "Cold"]  = df$hotDogScore[df['heat'] == "Cold"] - 1
df$hotDogScore[df['condiment'] == "Ketchup"] <- df$hotDogScore[df['condiment'] == "Ketchup"] + 1
df$hotDogScore[df['condiment'] == "Mustard"] <- df$hotDogScore[df['condiment'] == "Mustard"] + 2
df$hotDogScore[df['condiment'] == "Wasabi"] <- df$hotDogScore[df['condiment'] == "Wasabi"] - 1
df$hotDogScore[df['condiment'] == "Syrup"] <- df$hotDogScore[df['condiment'] == "Syrup"] - 4

# Add noise
df$hotDogScore <- df$hotDogScore + rnorm(n, mean = 0, sd = 1.25)

# Regression data set
dfR <- df
dfR$isHotDog <- NULL
dfR$vendorID <- NULL

# Set parameters for regression models
pR <- SupervisedModelDevelopmentParams$new()
pR$df <- dfR
pR$type <- "regression"
pR$impute <- TRUE
pR$grainCol <- "id"
pR$predictedCol <- "hotDogScore"
pR$debug <- FALSE
pR$cores <- 1

# Classification data set
dfC <- df
# assign response variable
dfC["isHotDog"] <- ifelse(dfC["hotDogScore"] > 0, "Y", "N")
dfC$isHotDog <- as.character(dfC$isHotDog)
dfC$hotDogScore <- NULL
dfC$vendorID <- NULL

# Set parameters for classification models
pC <- SupervisedModelDevelopmentParams$new()
pC$df <- dfC
pC$type <- "classification"
pC$impute <- TRUE
pC$grainCol <- "id"
pC$predictedCol <- "isHotDog"
pC$debug <- FALSE
pC$cores <- 1

# LMM data set
dfLMM <- df
# Shift values based on vendorID: higher vendorID -> greater chance of hotdog
dfLMM$hotDogScore <- dfLMM$hotDogScore + rnorm(n, 
                                               mean = (dfLMM$vendorID - 5)/2, 
                                               sd = 0.25)
# assign response varaible
dfLMM["isHotDog"] <- ifelse(dfLMM["hotDogScore"] > 0, "Y", "N")
dfLMM$isHotDog <- as.character(dfLMM$isHotDog)
dfLMM$hotDogScore <- NULL

# Set parameters for LMM
pL <- SupervisedModelDevelopmentParams$new()
pL$df <- dfLMM
pL$type <- "classification"
pL$impute <- TRUE
pL$grainCol <- "id"
pL$personCol <- "vendorID"
pL$predictedCol <- "isHotDog"
pL$debug <- FALSE
pL$cores <- 1

# reset seed
set.seed(NULL)

#### DEVELOP CLASSIFICATION MODELS ####

set.seed(7)
RandomForestC <- RandomForestDevelopment$new(pC)
capture.output(RandomForestC$run())
LassoC <- LassoDevelopment$new(pC)
capture.output(LassoC$run())
set.seed(NULL)

#### CLASSIFICATION TESTS ####

test_that("Single row predictions work for RF classification", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "classification"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "isHotDog"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Deploy rf on single row
  capture.output(rfD1 <- RandomForestDeployment$new(p1))
  rfD1$deploy()
  
  # Get prediction
  rfOutDf1 <- rfD1$getOutDf()
  
  # Check that a prediction is made
  expect_equal(rfOutDf1$id[1], 9001)
  expect_is(rfOutDf1$PredictedProbNBR[1], "numeric")
})

test_that("Single row predictions work for lasso classification", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "classification"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "isHotDog"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Deploy lasso on single row
  capture.output(lassoD1 <- LassoDeployment$new(p1))
  lassoD1$deploy()
  
  # Get prediction
  lassoOutDf1 <- lassoD1$getOutDf()
  
  # Check that a prediction is made
  expect_equal(lassoOutDf1$id[1], 9001)
  expect_is(lassoOutDf1$PredictedProbNBR[1], "numeric")
})

test_that("RF classification predictions are independent of each other", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "classification"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "isHotDog"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Two row prediction data set, with first row same as dfDeploy2
  dfDeploy2 <- data.frame(id = c(9001, 9002),
                          length = c(6, 2),
                          diameter = c(3, 2), 
                          heat = c("Cold", "Hot"),
                          condiment = c("Syrup", "Mustard"))
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeploy2
  p2$grainCol <- "id"
  p2$predictedCol <- "isHotDog"
  p2$impute <- TRUE
  p2$debug <- F
  p2$cores <- 1
  
  # Full develop data set with 1 new row
  dfC2 <- dfC
  dfC2$isHotDog <- NULL
  dfFull <- rbind(dfDeploy1, dfC2)
  
  pF <- SupervisedModelDeploymentParams$new()
  pF$type <- "classification"
  pF$df <- dfFull
  pF$grainCol <- "id"
  pF$predictedCol <- "isHotDog"
  pF$impute <- TRUE
  pF$debug <- F
  pF$cores <- 1
  
  # Deploy rf on single row
  capture.output(rfD1 <- RandomForestDeployment$new(p1))
  rfD1$deploy()
  
  rfOutDf1 <- rfD1$getOutDf()
  
  # Deploy rf on 2 rows, one of which is identical to the one above
  capture.output(rfD2 <- RandomForestDeployment$new(p2))
  rfD2$deploy()
  
  rfOutDf2 <- rfD2$getOutDf()
  
  # Deploy rf on full develop set plus single new row
  capture.output(rfDFull <- RandomForestDeployment$new(pF))
  rfDFull$deploy()
  
  rfOutDfFull <- rfDFull$getOutDf()
  
  # Check that the predictions are the same
  expect_equal(rfOutDf1[1, ]$PredictedProbNBR, rfOutDf2[1, ]$PredictedProbNBR)
  expect_equal(rfOutDf1[1, ]$PredictedProbNBR, rfOutDfFull[1, ]$PredictedProbNBR)
})

test_that("Lasso classification predictions are independent of each other", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "classification"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "isHotDog"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Two row prediction data set, with first row same as dfDeploy2
  dfDeploy2 <- data.frame(id = c(9001, 9002),
                          length = c(6, 2),
                          diameter = c(3, 2), 
                          heat = c("Cold", "Hot"),
                          condiment = c("Syrup", "Mustard"))
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeploy2
  p2$grainCol <- "id"
  p2$predictedCol <- "isHotDog"
  p2$impute <- TRUE
  p2$debug <- F
  p2$cores <- 1
  
  # Full develop data set with 1 new row
  dfC2 <- dfC
  dfC2$isHotDog <- NULL
  dfFull <- rbind(dfDeploy1, dfC2)
  
  pF <- SupervisedModelDeploymentParams$new()
  pF$type <- "classification"
  pF$df <- dfFull
  pF$grainCol <- "id"
  pF$predictedCol <- "isHotDog"
  pF$impute <- TRUE
  pF$debug <- F
  pF$cores <- 1
  
  # Deploy lasso on single row
  capture.output(lassoD1 <- LassoDeployment$new(p1))
  lassoD1$deploy()
  
  lassoOutDf1 <- lassoD1$getOutDf()
  
  # Deploy lasso on 2 rows, one of which is identical to the one above
  capture.output(lassoD2 <- LassoDeployment$new(p2))
  lassoD2$deploy()
  
  lassoOutDf2 <- lassoD2$getOutDf()
  
  # Deploy lasso on full develop set plus single new row
  capture.output(lassoDFull <- LassoDeployment$new(pF))
  lassoDFull$deploy()
  
  lassoOutDfFull <- lassoDFull$getOutDf()
  
  # Check that the predictions are the same
  expect_equal(lassoOutDf1[1, ]$PredictedProbNBR, lassoOutDf2[1, ]$PredictedProbNBR)
  expect_equal(lassoOutDf1[1, ]$PredictedProbNBR, lassoOutDfFull[1, ]$PredictedProbNBR)
})

test_that("Extra factors are imputed correctly for rf classification (1 column)", {
  # Data set with new factor level in one column
  # Need last row for imputation
  dfDeploy3 <- data.frame(id = c(9003, 9004, 9005),
                          length = c(6, 6, 6),
                          diameter = c(3, 3, 3), 
                          heat = c("Frozen", NA, "Cold"),
                          condiment = c("Ketchup", "Ketchup", "Ketchup"))
  
  p3 <- SupervisedModelDeploymentParams$new()
  p3$type <- "classification"
  p3$df <- dfDeploy3
  p3$grainCol <- "id"
  p3$predictedCol <- "isHotDog"
  p3$impute <- TRUE
  p3$debug <- F
  p3$cores <- 1
  
  # Deploy on 3 rows, which should yield the same predictions
  capture.output(rfD3 <- RandomForestDeployment$new(p3))
  
  # Check that a warning reporting new factor levels is triggered
  expect_warning(rfD3$deploy(), 
                 paste("New categorical variable levels were found:\n",
                       " -  heat : Frozen\n",
                       "These values have been set to NA.",
                       sep = ""))
  
  rfOutDf3 <- rfD3$getOutDf()
  
  # Check that new value is treated as NA
  expect_equal(rfOutDf3[1, ]$PredictedProbNBR, rfOutDf3[2, ]$PredictedProbNBR)
})

test_that("Extra factors are imputed correctly for lasso classification  (1 column)", {
  # Data set with new factor level in one column
  # Need last row for imputation
  dfDeploy3 <- data.frame(id = c(9003, 9004, 9005),
                          length = c(6, 6, 6),
                          diameter = c(3, 3, 3), 
                          heat = c("Frozen", NA, "Cold"),
                          condiment = c("Ketchup", "Ketchup", "Ketchup"))
  
  p3 <- SupervisedModelDeploymentParams$new()
  p3$type <- "classification"
  p3$df <- dfDeploy3
  p3$grainCol <- "id"
  p3$predictedCol <- "isHotDog"
  p3$impute <- TRUE
  p3$debug <- F
  p3$cores <- 1
  
  # Deploy on 3 rows, which should yield the same predictions
  capture.output(lassoD3 <- LassoDeployment$new(p3))
  # Check that a warning reporting new factor levels is triggered
  expect_warning(lassoD3$deploy(), 
                 paste("New categorical variable levels were found:\n",
                       " -  heat : Frozen\n",
                       "These values have been set to NA.",
                       sep = ""))
  
  lassoOutDf3 <- lassoD3$getOutDf()
  
  # Check that new value is treated as NA
  expect_equal(lassoOutDf3[1, ]$PredictedProbNBR, lassoOutDf3[2, ]$PredictedProbNBR)
})

test_that("Extra factors are imputed correctly for rf classification (2 columns)", {
  # Data set with new factor levels in two columns
  # Need last row for imputation
  dfDeploy4 <- data.frame(id = c(9006, 9007, 9008, 9009),
                          length = c(5, 5, 5, 5),
                          diameter = c(2, 2, 2, 2), 
                          heat = c("Lukewarm", "Caliente", NA, "Hot"),
                          condiment = c("Chili", "Fudge", NA, "Wasabi"))
  
  p4 <- SupervisedModelDeploymentParams$new()
  p4$type <- "classification"
  p4$df <- dfDeploy4
  p4$grainCol <- "id"
  p4$predictedCol <- "isHotDog"
  p4$impute <- TRUE
  p4$debug <- F
  p4$cores <- 1
  
  # Deploy on 4 rows, which should yield the same predictions
  capture.output(rfD4 <- RandomForestDeployment$new(p4))
  
  # Check that a warning reporting new factor levels is triggered
  # Use regular expression to deal with weird quote issues
  expect_warning(rfD4$deploy(), 
                 regex = paste("New categorical variable levels were found:\n",
                               " -  heat : .{3}Caliente.{4}Lukewarm.{2}\n",
                               " -  condiment : .{3}Chili.{4}Fudge.{2}\n", 
                               "These values have been set to NA.",
                               sep = ""))
  
  rfOutDf4 <- rfD4$getOutDf()
  
  # Check that new value is treated as NA
  expect_equal(rfOutDf4[1, ]$PredictedProbNBR, rfOutDf4[3, ]$PredictedProbNBR)
  expect_equal(rfOutDf4[2, ]$PredictedProbNBR, rfOutDf4[3, ]$PredictedProbNBR)
})

test_that("Extra factors are imputed correctly for lasso classification (2 columns)", {
  # Data set with new factor levels in two columns
  # Need last row for imputation
  dfDeploy4 <- data.frame(id = c(9006, 9007, 9008, 9009),
                          length = c(5, 5, 5, 5),
                          diameter = c(2, 2, 2, 2), 
                          heat = c("Lukewarm", "Caliente", NA, "Hot"),
                          condiment = c("Chili", "Fudge", NA, "Wasabi"))
  
  p4 <- SupervisedModelDeploymentParams$new()
  p4$type <- "classification"
  p4$df <- dfDeploy4
  p4$grainCol <- "id"
  p4$predictedCol <- "isHotDog"
  p4$impute <- TRUE
  p4$debug <- F
  p4$cores <- 1
  
  # Deploy on 4 rows, which should yield the same predictions
  capture.output(lassoD4 <- LassoDeployment$new(p4))
  
  # Check that a warning reporting new factor levels is triggered
  # Use regular expression to deal with weird quote issues
  expect_warning(lassoD4$deploy(), 
                 regex = paste("New categorical variable levels were found:\n",
                               " -  heat : .{3}Caliente.{4}Lukewarm.{2}\n",
                               " -  condiment : .{3}Chili.{4}Fudge.{2}\n", 
                               "These values have been set to NA.",
                               sep = ""))
  
  lassoOutDf4 <- lassoD4$getOutDf()
  
  # Check that new value is treated as NA
  expect_equal(lassoOutDf4[1, ]$PredictedProbNBR, lassoOutDf4[3, ]$PredictedProbNBR)
  expect_equal(lassoOutDf4[2, ]$PredictedProbNBR, lassoOutDf4[3, ]$PredictedProbNBR)
})

test_that("Single row predictions work when NAs are present (RF classifier)", {
  # Dataframe with single row and missing value
  dfDeploy5 <- data.frame(id = 9010,
                          length = 8,
                          diameter = NA,
                          heat = "Hot",
                          condiment = NA)

  p5 <- SupervisedModelDeploymentParams$new()
  p5$type <- "classification"
  p5$df <- dfDeploy5
  p5$grainCol <- "id"
  p5$predictedCol <- "isHotDog"
  p5$impute <- TRUE
  p5$debug <- F
  p5$cores <- 1
  
  # Deploy the model
  capture.output(rfD5 <- RandomForestDeployment$new(p5))
  capture.output(rfD5$deploy())
  rfOutDf5 <- rfD5$getOutDf()
  
  # Check that a prediction is made
  expect_equal(rfOutDf5$id[1], 9010)
  expect_is(rfOutDf5$PredictedProbNBR[1], "numeric")
})



#### DEVELOP REGRESSION MODELS ####
# Need to develop regression models after classification tests.  Otherwise
# the model will be overwritten (only saves one rf model and one lasso model at 
# a time)
set.seed(7)
RandomForestR <- RandomForestDevelopment$new(pR)
capture.output(RandomForestR$run())
LassoR <- LassoDevelopment$new(pR)
capture.output(LassoR$run())
set.seed(NULL)

#### REGRESSION TESTS ####

test_that("Single row predictions work for RF regression", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "regression"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "hotDogScore"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Deploy rf on single row
  capture.output(rfD1 <- RandomForestDeployment$new(p1))
  rfD1$deploy()
  
  # Get prediction
  rfOutDf1 <- rfD1$getOutDf()
  
  # Check that a prediction is made
  expect_equal(rfOutDf1$id[1], 9001)
  expect_is(rfOutDf1$PredictedValueNBR[1], "numeric")
})

test_that("Single row predictions work for lasso regression", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "regression"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "hotDogScore"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Deploy lasso on single row
  capture.output(lassoD1 <- LassoDeployment$new(p1))
  lassoD1$deploy()
  
  # Get prediction
  lassoOutDf1 <- lassoD1$getOutDf()
  
  # Check that a prediction is made
  expect_equal(lassoOutDf1$id[1], 9001)
  expect_is(lassoOutDf1$PredictedValueNBR[1], "numeric")
})

test_that("RF regression predictions are independent of each other", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "regression"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "hotDogScore"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Two row prediction data set, with first row same as dfDeploy2
  dfDeploy2 <- data.frame(id = c(9001, 9002),
                          length = c(6, 2),
                          diameter = c(3, 2), 
                          heat = c("Cold", "Hot"),
                          condiment = c("Syrup", "Mustard"))
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "regression"
  p2$df <- dfDeploy2
  p2$grainCol <- "id"
  p2$predictedCol <- "hotDogScore"
  p2$impute <- TRUE
  p2$debug <- F
  p2$cores <- 1
  
  # Full develop data set with 1 new row
  dfR2 <- dfR
  dfR2$hotDogScore <- NULL
  dfFull <- rbind(dfDeploy1, dfR2)
  
  pF <- SupervisedModelDeploymentParams$new()
  pF$type <- "regression"
  pF$df <- dfFull
  pF$grainCol <- "id"
  pF$predictedCol <- "hotDogScore"
  pF$impute <- TRUE
  pF$debug <- F
  pF$cores <- 1
  
  # Deploy rf on single row
  capture.output(rfD1 <- RandomForestDeployment$new(p1))
  rfD1$deploy()
  
  rfOutDf1 <- rfD1$getOutDf()
  
  # Deploy rf on 2 rows, one of which is identical to the one above
  capture.output(rfD2 <- RandomForestDeployment$new(p2))
  rfD2$deploy()
  
  rfOutDf2 <- rfD2$getOutDf()
  
  # Deploy rf on full develop set plus single new row
  capture.output(rfDFull <- RandomForestDeployment$new(pF))
  rfDFull$deploy()
  
  rfOutDfFull <- rfDFull$getOutDf()
  
  # Check that the predictions are the same
  expect_equal(rfOutDf1[1, ]$PredictedValueNBR, rfOutDf2[1, ]$PredictedValueNBR)
  expect_equal(rfOutDf1[1, ]$PredictedValueNBR, rfOutDfFull[1, ]$PredictedValueNBR)
})

test_that("Lasso regression predictions are independent of each other", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "regression"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "hotDogScore"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Two row prediction data set, with first row same as dfDeploy2
  dfDeploy2 <- data.frame(id = c(9001, 9002),
                          length = c(6, 2),
                          diameter = c(3, 2), 
                          heat = c("Cold", "Hot"),
                          condiment = c("Syrup", "Mustard"))
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "regression"
  p2$df <- dfDeploy2
  p2$grainCol <- "id"
  p2$predictedCol <- "hotDogScore"
  p2$impute <- TRUE
  p2$debug <- F
  p2$cores <- 1
  
  # Full develop data set with 1 new row
  dfR2 <- dfR
  dfR2$hotDogScore <- NULL
  dfFull <- rbind(dfDeploy1, dfR2)
  
  pF <- SupervisedModelDeploymentParams$new()
  pF$type <- "regression"
  pF$df <- dfFull
  pF$grainCol <- "id"
  pF$predictedCol <- "hotDogScore"
  pF$impute <- TRUE
  pF$debug <- F
  pF$cores <- 1
  
  # Deploy lasso on single row
  capture.output(lassoD1 <- LassoDeployment$new(p1))
  lassoD1$deploy()
  
  lassoOutDf1 <- lassoD1$getOutDf()
  
  # Deploy lasso on 2 rows, one of which is identical to the one above
  capture.output(lassoD2 <- LassoDeployment$new(p2))
  lassoD2$deploy()
  
  lassoOutDf2 <- lassoD2$getOutDf()
  
  # Deploy lasso on full develop set plus single new row
  capture.output(lassoDFull <- LassoDeployment$new(pF))
  lassoDFull$deploy()
  
  lassoOutDfFull <- lassoDFull$getOutDf()
  
  # Check that the predictions are the same
  expect_equal(lassoOutDf1[1, ]$PredictedValueNBR, lassoOutDf2[1, ]$PredictedValueNBR)
  expect_equal(lassoOutDf1[1, ]$PredictedValueNBR, lassoOutDfFull[1, ]$PredictedValueNBR)
})

test_that("Extra factors are imputed correctly for rf regression (1 column)", {
  # Data set with new factor level in one column
  # Need last row for imputation
  dfDeploy3 <- data.frame(id = c(9003, 9004, 9005),
                          length = c(6, 6, 6),
                          diameter = c(3, 3, 3), 
                          heat = c("Frozen", NA, "Cold"),
                          condiment = c("Ketchup", "Ketchup", "Ketchup"))
  
  p3 <- SupervisedModelDeploymentParams$new()
  p3$type <- "regression"
  p3$df <- dfDeploy3
  p3$grainCol <- "id"
  p3$predictedCol <- "hotDogScore"
  p3$impute <- TRUE
  p3$debug <- F
  p3$cores <- 1
  
  # Deploy on 3 rows, which should yield the same predictions
  capture.output(rfD3 <- RandomForestDeployment$new(p3))
  
  # Check that a warning reporting new factor levels is triggered
  expect_warning(rfD3$deploy(), 
                 paste("New categorical variable levels were found:\n",
                       " -  heat : Frozen\n",
                       "These values have been set to NA.",
                       sep = ""))
  
  rfOutDf3 <- rfD3$getOutDf()
  
  # Check that new value is treated as NA
  expect_equal(rfOutDf3[1, ]$PredictedValueNBR, rfOutDf3[2, ]$PredictedValueNBR)
})

test_that("Extra factors are imputed correctly for lasso regression (1 column)", {
  # Data set with new factor level in one column
  # Need last row for imputation
  dfDeploy3 <- data.frame(id = c(9003, 9004, 9005),
                          length = c(6, 6, 6),
                          diameter = c(3, 3, 3), 
                          heat = c("Frozen", NA, "Cold"),
                          condiment = c("Ketchup", "Ketchup", "Ketchup"))
  
  p3 <- SupervisedModelDeploymentParams$new()
  p3$type <- "regression"
  p3$df <- dfDeploy3
  p3$grainCol <- "id"
  p3$predictedCol <- "hotDogScore"
  p3$impute <- TRUE
  p3$debug <- F
  p3$cores <- 1
  
  # Deploy on 3 rows, which should yield the same predictions
  capture.output(lassoD3 <- LassoDeployment$new(p3))
  # Check that a warning reporting new factor levels is triggered
  expect_warning(lassoD3$deploy(), 
                 paste("New categorical variable levels were found:\n",
                       " -  heat : Frozen\n",
                       "These values have been set to NA.",
                       sep = ""))
  
  lassoOutDf3 <- lassoD3$getOutDf()
  
  # Check that new value is treated as NA
  expect_equal(lassoOutDf3[1, ]$PredictedValueNBR, lassoOutDf3[2, ]$PredictedValueNBR)
})

test_that("Extra factors are imputed correctly for rf regression (2 columns)", {
  # Data set with new factor levels in two columns
  # Need last row for imputation
  dfDeploy4 <- data.frame(id = c(9006, 9007, 9008, 9009),
                          length = c(5, 5, 5, 5),
                          diameter = c(2, 2, 2, 2), 
                          heat = c("Lukewarm", "Caliente", NA, "Hot"),
                          condiment = c("Chili", "Fudge", NA, "Wasabi"))
  
  p4 <- SupervisedModelDeploymentParams$new()
  p4$type <- "regression"
  p4$df <- dfDeploy4
  p4$grainCol <- "id"
  p4$predictedCol <- "hotDogScore"
  p4$impute <- TRUE
  p4$debug <- F
  p4$cores <- 1
  
  # Deploy on 4 rows, which should yield the same predictions
  capture.output(rfD4 <- RandomForestDeployment$new(p4))
  
  # Check that a warning reporting new factor levels is triggered
  # Use regular expression to deal with weird quote issues
  expect_warning(rfD4$deploy(), 
                 regex = paste("New categorical variable levels were found:\n",
                               " -  heat : .{3}Caliente.{4}Lukewarm.{2}\n",
                               " -  condiment : .{3}Chili.{4}Fudge.{2}\n", 
                               "These values have been set to NA.",
                               sep = ""))
  
  rfOutDf4 <- rfD4$getOutDf()
  
  # Check that new value is treated as NA
  expect_equal(rfOutDf4[1, ]$PredictedValueNBR, rfOutDf4[3, ]$PredictedValueNBR)
  expect_equal(rfOutDf4[2, ]$PredictedValueNBR, rfOutDf4[3, ]$PredictedValueNBR)
})

test_that("Extra factors are imputed correctly for lasso regression (2 columns)", {
  # Data set with new factor levels in two columns
  # Need last row for imputation
  dfDeploy4 <- data.frame(id = c(9006, 9007, 9008, 9009),
                          length = c(5, 5, 5, 5),
                          diameter = c(2, 2, 2, 2), 
                          heat = c("Lukewarm", "Caliente", NA, "Hot"),
                          condiment = c("Chili", "Fudge", NA, "Wasabi"))
  
  p4 <- SupervisedModelDeploymentParams$new()
  p4$type <- "regression"
  p4$df <- dfDeploy4
  p4$grainCol <- "id"
  p4$predictedCol <- "hotDogScore"
  p4$impute <- TRUE
  p4$debug <- F
  p4$cores <- 1
  
  # Deploy on 4 rows, which should yield the same predictions
  capture.output(lassoD4 <- LassoDeployment$new(p4))
  
  # Check that a warning reporting new factor levels is triggered
  # Use regular expression to deal with weird quote issues
  expect_warning(lassoD4$deploy(), 
                 regex = paste("New categorical variable levels were found:\n",
                               " -  heat : .{3}Caliente.{4}Lukewarm.{2}\n",
                               " -  condiment : .{3}Chili.{4}Fudge.{2}\n", 
                               "These values have been set to NA.",
                               sep = ""))
  
  lassoOutDf4 <- lassoD4$getOutDf()
  
  # Check that new value is treated as NA
  expect_equal(lassoOutDf4[1, ]$PredictedValueNBR, lassoOutDf4[3, ]$PredictedValueNBR)
  expect_equal(lassoOutDf4[2, ]$PredictedValueNBR, lassoOutDf4[3, ]$PredictedValueNBR)
})

test_that("Single row predictions work when NAs are present (RF regrssor)", {
  # Dataframe with single row and missing value
  dfDeploy5 <- data.frame(id = 9010,
                          length = 8,
                          diameter = NA,
                          heat = "Hot",
                          condiment = NA)
  
  p5 <- SupervisedModelDeploymentParams$new()
  p5$type <- "regression"
  p5$df <- dfDeploy5
  p5$grainCol <- "id"
  p5$predictedCol <- "hotDogScore"
  p5$impute <- TRUE
  p5$debug <- F
  p5$cores <- 1
  
  # Deploy the model
  capture.output(rfD5 <- RandomForestDeployment$new(p5))
  capture.output(rfD5$deploy())
  rfOutDf5 <- rfD5$getOutDf()
  
  # Check that a prediction is made
  expect_equal(rfOutDf5$id[1], 9010)
  expect_is(rfOutDf5$PredictedValueNBR[1], "numeric")
})



#### DEVELOP LMM CLASSIFICATION MODEL ####
set.seed(7)
LMM <- LinearMixedModelDevelopment$new(pL)
capture.output(LMM$run())
set.seed(NULL)

#### LMM TESTS ####

test_that("Single row predictions work for LMM classification", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          vendorID = 9,
                          length = 8,
                          diameter = 2,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "classification"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$personCol <- "vendorID"
  p1$predictedCol <- "isHotDog"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Deploy LMM on single row
  capture.output(LMMD1 <- LinearMixedModelDeployment$new(p1))
  LMMD1$deploy()
  
  # Get prediction
  LMMOutDf1 <- LMMD1$getOutDf()
  
  # Check that a prediction is made
  expect_equal(LMMOutDf1$id[1], 9001)
  expect_is(LMMOutDf1$PredictedProbNBR[1], "numeric")
})

test_that("LMM classification predictions are independent of each other", {
  # Single row prediction data set
  dfDeploy1 <- data.frame(id = 9001,
                          vendorID = 9,
                          length = 8,
                          diameter = 2,
                          heat = "Cold",
                          condiment = "Syrup")
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "classification"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$personCol <- "vendorID"
  p1$predictedCol <- "isHotDog"
  p1$impute <- TRUE
  p1$debug <- F
  p1$cores <- 1
  
  # Two row prediction data set, with first row same as dfDeploy2
  dfDeploy2 <- data.frame(id = c(9001, 9002),
                          vendorID = c(9, 3), 
                          length = c(8, 2),
                          diameter = c(2, 2), 
                          heat = c("Cold", "Hot"),
                          condiment = c("Syrup", "Mustard"))
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeploy2
  p2$grainCol <- "id"
  p2$personCol <- "vendorID"
  p2$predictedCol <- "isHotDog"
  p2$impute <- TRUE
  p2$debug <- F
  p2$cores <- 1
  
  # Full develop data set with 1 new row
  dfLMM2 <- dfLMM
  dfLMM2$isHotDog <- NULL
  dfFull <- rbind(dfDeploy1, dfLMM2)
  
  pF <- SupervisedModelDeploymentParams$new()
  pF$type <- "classification"
  pF$df <- dfFull
  pF$grainCol <- "id"
  pF$personCol <- "vendorID"
  pF$predictedCol <- "isHotDog"
  pF$impute <- TRUE
  pF$debug <- F
  pF$cores <- 1
  
  # Deploy LMM on single row
  capture.output(LMMD1 <- LinearMixedModelDeployment$new(p1))
  LMMD1$deploy()
  
  LMMOutDf1 <- LMMD1$getOutDf()
  
  # Deploy rf on 2 rows, one of which is identical to the one above
  capture.output(LMMD2 <- LinearMixedModelDeployment$new(p2))
  LMMD2$deploy()
  
  LMMOutDf2 <- LMMD2$getOutDf()
  
  # Deploy lasso on full develop set plus single new row
  capture.output(LMMDFull <- LinearMixedModelDeployment$new(pF))
  LMMDFull$deploy()
  
  LMMOutDfFull <- LMMDFull$getOutDf()
  
  # Check that the predictions are the same
  expect_equal(LMMOutDf1[1, ]$PredictedProbNBR, LMMOutDf2[1, ]$PredictedProbNBR)
  expect_equal(LMMOutDf1[1, ]$PredictedProbNBR, LMMOutDfFull[1, ]$PredictedProbNBR)
})

test_that("Extra factors are imputed correctly for LMM classification (1 column)", {
  # Data set with new factor level in one column
  # Need last row for imputation
  dfDeploy3 <- data.frame(id = c(9003, 9004, 9005),
                          vendorID = c(9,9,9),
                          length = c(8, 8, 8),
                          diameter = c(2, 2, 2),
                          heat = c("Frozen", NA, "Cold"),
                          condiment = c("Ketchup", "Ketchup", "Ketchup"))

  p3 <- SupervisedModelDeploymentParams$new()
  p3$type <- "classification"
  p3$df <- dfDeploy3
  p3$grainCol <- "id"
  p3$personCol <- "vendorID"
  p3$predictedCol <- "isHotDog"
  p3$impute <- TRUE
  p3$debug <- F
  p3$cores <- 1

  # Deploy on 3 rows, which should yield the same predictions
  capture.output(LMMD3 <- LinearMixedModelDeployment$new(p3))

  # Check that a warning reporting new factor levels is triggered
  expect_warning(LMMD3$deploy(),
                 paste("New categorical variable levels were found:\n",
                       " -  heat : Frozen\n",
                       "These values have been set to NA.",
                       sep = ""))

  LMMOutDf3 <- LMMD3$getOutDf()

  # Check that new value is treated as NA
  expect_equal(LMMOutDf3[1, ]$PredictedProbNBR, LMMOutDf3[2, ]$PredictedProbNBR)
})

test_that("Extra factors are imputed correctly for LMM classification (2 columns)", {
  # Data set with new factor levels in two columns
  # Need last row for imputation
  dfDeploy4 <- data.frame(id = c(9006, 9007, 9008, 9009),
                          vendorID = c(7, 7, 7, 7),
                          length = c(5, 5, 5, 5),
                          diameter = c(2, 2, 2, 2),
                          heat = c("Lukewarm", "Caliente", NA, "Hot"),
                          condiment = c("Chili", "Fudge", NA, "Wasabi"))

  p4 <- SupervisedModelDeploymentParams$new()
  p4$type <- "classification"
  p4$df <- dfDeploy4
  p4$grainCol <- "id"
  p4$personCol <- "vendorID"
  p4$predictedCol <- "isHotDog"
  p4$impute <- TRUE
  p4$debug <- F
  p4$cores <- 1

  # Deploy on 4 rows, which should yield the same predictions
  capture.output(LMMD4 <- LinearMixedModelDeployment$new(p4))

  # Check that a warning reporting new factor levels is triggered
  # Use regular expression to deal with weird quote issues
  expect_warning(LMMD4$deploy(),
                 regex = paste("New categorical variable levels were found:\n",
                               " -  heat : .{3}Caliente.{4}Lukewarm.{2}\n",
                               " -  condiment : .{3}Chili.{4}Fudge.{2}\n",
                               "These values have been set to NA.",
                               sep = ""))

  LMMOutDf4 <- LMMD4$getOutDf()

  # Check that new value is treated as NA
  expect_equal(LMMOutDf4[1, ]$PredictedProbNBR, LMMOutDf4[3, ]$PredictedProbNBR)
  expect_equal(LMMOutDf4[2, ]$PredictedProbNBR, LMMOutDf4[3, ]$PredictedProbNBR)
})

test_that("Single row predictions work when NAs are present (LMM)", {
  # Dataframe with single row and missing value
  dfDeploy5 <- data.frame(id = 9010, 
                          vendorID  = 5,
                          length = 8,
                          diameter = NA,
                          heat = "Hot",
                          condiment = NA)
  
  p5 <- SupervisedModelDeploymentParams$new()
  p5$type <- "classification"
  p5$df <- dfDeploy5
  p5$grainCol <- "id"
  p5$personCol <- "vendorID"
  p5$predictedCol <- "isHotDog"
  p5$impute <- TRUE
  p5$debug <- F
  p5$cores <- 1
  
  # Deploy the model
  capture.output(LMMD5 <- LinearMixedModelDeployment$new(p5))
  capture.output(LMMD5$deploy())
  LMMOutDf5 <- LMMD5$getOutDf()
  
  # Check that a prediction is made
  expect_equal(LMMOutDf5$id[1], 9010)
  expect_is(LMMOutDf5$PredictedProbNBR[1], "numeric")
})

test_that("Y and N aren't swapped when label is a character", {
  # Set up training data
  set.seed(1)
  n <- 200
  d <- data.frame(id = 1:n,
                  x = rnorm(n),
                  y = rnorm(n),
                  z = rnorm(n))
  # Add response variable
  d['positive'] <- ifelse(d$x + d$y + d$z + rnorm(n) > 0, "Y", "N")
  d['negative'] <- ifelse(d$positive == "Y", "N", "Y")
  
  # New data: first row should be classified as non-positive/negative, 
  # second row as positive/non-negative
  dDeploy <- data.frame(id = c(-1,1), 
                        x = c(-1, 1), 
                        y = c(-1, 1), 
                        z = c(-1, 1))
  
  # Build first model to check if positive
  dDevelop1 <- d
  dDevelop1$negative <- NULL
  
  # Build first model
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- dDevelop1
  p$type <- "classification"
  p$predictedCol <- 'positive'
  p$grainCol <- "id"
  p$impute <- TRUE
  p$debug <- F
  p$cores <- 1
  
  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$df <- dDeploy
  p2$type <- "classification"
  p2$predictedCol <- "positive"
  p2$grainCol <- "id"
  p2$impute <- TRUE
  p2$debug <- F
  p2$cores <- 1
  
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  # get first model's predictions
  outDf1 <- rfD$getOutDf()
  
  # Build second model
  dDevelop2 <- d
  dDevelop2$positive <- NULL
  
  p$df <- dDevelop2
  p$predictedCol <- "negative"
  
  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())
  
  p2$predictedCol <- "negative"
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  # get second model's predictions
  outDf2 <- rfD$getOutDf()
  
  # Check that predictions are 1) non-positive and 2) positive
  expect_true(outDf1$PredictedProbNBR[2] > outDf1$PredictedProbNBR[1])
  
  # Check that predictions are 1) negative and 2) non-negative
  expect_true(outDf2$PredictedProbNBR[1] > outDf2$PredictedProbNBR[2])
})

test_that("Y and N aren't swapped label is a factor", {
  # Set up training data
  set.seed(1)
  n <- 200
  d <- data.frame(id = 1:n,
                  x = rnorm(n),
                  y = rnorm(n),
                  z = rnorm(n))
  # Add response variable
  d$positive <- factor(ifelse(d$x + d$y + d$z + rnorm(n) > 0, "Y", "N"))

  dDeploy <- data.frame(id = c(-1,1), 
                        x = c(-1, 1), 
                        y = c(-1, 1), 
                        z = c(-1, 1))
  
  # Build first model
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- d
  p$type <- "classification"
  p$predictedCol <- 'positive'
  p$grainCol <- "id"
  p$impute <- TRUE
  p$debug <- F
  p$cores <- 1
  
  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$df <- dDeploy
  p2$type <- "classification"
  p2$predictedCol <- "positive"
  p2$grainCol <- "id"
  p2$impute <- TRUE
  p2$debug <- F
  p2$cores <- 1
  
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  # get first model's predictions
  outDf1 <- rfD$getOutDf()
  
  # Build second model, setting factor levels in reverse order
  d$positive <- relevel(d$positive, "Y")

  p$df <- d
  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())
  
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  # get second model's predictions
  outDf2 <- rfD$getOutDf()
  
  # Predictions are very different for the two rows in deployment,
  # so just test that the greater of the two is the same for 
  # each developed model
  expect_equal(order(outDf1$PredictedProbNBR),
               order(outDf2$PredictedProbNBR))
})
