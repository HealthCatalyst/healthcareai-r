context("Checking xgboost development")

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

# Get output data 
junk <- capture.output(xPred <- boost$getPredictions())

# Get raw predictions
xConf <- capture.output(boost$generateConfusionMatrix())

###########
# Multiclass

test_that("Number of classes are calculated correctly", {
  expect_true(xNew[9] == " $ num_class  : int 6")
})

test_that("Accuracy is the same every time", {
  expect_true(xRun[18] == "               Accuracy : 0.9577          ")
})

test_that("Predictions are the same every time", {
  expect_true(round(xPred[1,6],5) == 0.86515)
})

test_that("Grain column is inserted correctly", {
  expect_true(xPred[2,1] == 13)
})

test_that("Columns are mapped correctly", {
  expect_true(xPred[2,9] == "two")
})

test_that("Max probability is found correctly", {
  expect_true(xPred[2,8] == "two")
})

test_that("Confusion matrix accuracy is the same every time", {
  expect_true(xConf[15] == "               Accuracy : 0.9577          ")
})

