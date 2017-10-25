# The tests in this file test the getProcessVariablesDf in 
# SupervisedModelDeployment.R

context("Checking getProcessVariablesDf")

# Setup: Build Random Forest Model ---------------------------------------------
n <- 250
d <- data.frame(id = 1:n, 
                x = rnorm(n),
                y = sample(c("A", "B", "C"), replace = TRUE, size = n),
                z = sample(c("pie", "cake"), replace = TRUE, size = n))
# Assign a response variable
temp <- d$x + 
  1.5 * (d$y == "A") - 1.5 * (d$y == "C") +
  (d$z == "pie") - (d$z == "cake")
d$target <- ifelse(temp + rnorm(n) > 0, "Y", "N")

# Develop a random forest model
p <- SupervisedModelDevelopmentParams$new()
p$df <- d
p$type <- "classification"
p$predictedCol <- "target"
p$grainCol <- "id"
p$cores <- 1
capture.output(rf <- RandomForestDevelopment$new(p))
ignoreSpecWarn(capture.output(rf$run()),
               wRegexps = "ROC curve with AUC == 1")

# Set up development parameters 
p2 <- SupervisedModelDeploymentParams$new()
p2$df <- d
p2$type <- "classification"
p2$predictedCol <- "target"
p2$grainCol <- "id"
p2$cores <- 1

# Begin Tests: test that the correct variables are dropped ---------------------

test_that("variables not present in the data are dropped", {
  # Deploy in test to avoid warning
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  # Add modifiable variables not present in the data
  extra_vars <- c("y", "z", "non-existant", "ghost")
  
  # Check that a warning is triggered if new modifiable process variables 
  # are introduced
  expect_warning(capture.output(pvdf <- rfD$getProcessVariablesDf(extra_vars)),
                 "Some of the modifiable process variables specified are not ",
                 "present in the data. Mystery variables:\n",
                 " - non-existant\n",
                 " - ghost\n",
                 "These modifiable variables will not be used.")
  
  # Check that only y and z were considered as modifiable factors by checking 
  # (1) the number of columns (grain and predictiction + 5 for each modifiable 
  # variable)
  expect_equal(ncol(pvdf), 2 + 5*2)
  # and (2) the names of the modifiable variables in the output dataframe
  values <- union(unique(pvdf$Modify1TXT), unique(pvdf$Modify2TXT))
  expect_true(setequal(values, c("y", "z")))
})
  
test_that("non-categorical variables are dropped", {
  # Deploy in test to avoid warning
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  # Add a modifiable variable which is not categorical
  with_nums <- c("x", "y", "z")
  
  # Check that a warning is triggered if one of the modifiable variables is
  # not actually a categorical variable.
  expect_warning(capture.output(pvdf2 <- rfD$getProcessVariablesDf(with_nums)),
                 "Modifiable process variables must either be categorical ",
                 "variables or you must explicitly specify the levels. The ", 
                 "following variables are not categorical and will not be ",
                 "used:\n",
                 " - x")
  
  # Check that only y and z were considered as modifiable factors by checking 
  # (1) the number of columns (grain and predictiction + 5 for each modifiable 
  # variable)
  expect_equal(ncol(pvdf2), 2 + 5*2)
  # and (2) the names of the modifiable variables in the output dataframe
  values <- union(unique(pvdf2$Modify1TXT), unique(pvdf2$Modify2TXT))
  expect_true(setequal(values, c("y", "z")))
})

# Setup: Build Lasso Model -----------------------------------------------------

# Build a dataset with one useless categorical variable
d <- data.frame(id = 1:12,
                x = c(0, 2, 1, 2, 0, 3, 0, 3, 3, 4, 2, 4),
                y = c("down", "down", "down", "down", "up", "down",
                      "up", "down", "up", "up", "up", "down"),
                useless = rep(c("huh", "meh"), times = 6), # lasso will ignore
                target = c(rep("N", times = 6), rep(c("Y"), times = 6)))
# Make the dataset large enough to avoid triggering certain warnings
d <- d[rep(1:12, times = 5), ]

# Develop a lasso forest model
p <- SupervisedModelDevelopmentParams$new()
p$df <- d
p$type <- "classification"
p$predictedCol <- "target"
p$grainCol <- "id"
p$cores <- 1
capture.output(lasso <- LassoDevelopment$new(p))
# supress warning that is unrelated to the unit test
ignoreSpecWarn(capture.output(lasso$run()),
               wRegexps = c("glm.fit: fitted probabilities numerically 0 or 1",
                            "ROC curve with AUC == 1"))


# Set up development parameters with one useless modifiable variable
p2 <- SupervisedModelDeploymentParams$new()
p2$df <- d
p2$type <- "classification"
p2$predictedCol <- "target"
p2$grainCol <- "id"
p2$cores <- 1

# Resume Tests: test that the correct variables are dropped with lasso ---------

test_that("variables with non-zero lasso coefficient are recorded", {
  # Check that the useless variable is dropped
  expect_equal(lasso$modelInfo$usedVariables, c("x", "y"))
})

test_that("variables with zero coefficient in lasso are dropped", {
  # Deploy in test to avoid warning
  capture.output(lassoD <- LassoDeployment$new(p2))
  capture.output(lassoD$deploy())
  # Include variable with 0 coefficient in lasso model
  with_useless <- c("y", "useless")
  
  # Check warning of useless variable being dropped is triggered
  expect_warning(
    capture.output(pvdf3 <- lassoD$getProcessVariablesDf(with_useless)),
    "The following variables have coefficients of 0 in the lasso ",
    "model and will not be used as modifiable variables:\n",
    " - useless"
  )
  
  # Check that only y and z were considered as modifiable factors by checking 
  # (1) the number of columns (grain and predictiction + 5 for each modifiable 
  # variable)
  expect_equal(ncol(pvdf3), 2 + 5*1)
  # and (2) the names of the modifiable variables in the output dataframe
  values <- union(unique(pvdf3$Modify1TXT), unique(pvdf3$Modify2TXT))
  expect_true(setequal(values, c("y")))
})

test_that("error is triggered when no valid variables are used", {
  # Deploy in test to avoid warning
  capture.output(lassoD <- LassoDeployment$new(p2))
  capture.output(lassoD$deploy())
  # include new variables, non-categorical variables, and variables with zero
  # coefficient in the lasso model.
  all_bad <- c("non-existant", "ghost", "x", "useless")
  
  # Check that error is triggered if none of the modifiable variables is valid
  warnings <- capture_warnings(
    expect_error(capture.output(lassoD$getProcessVariablesDf(all_bad)),
                 "No valid modifiable variables used.")
  )
  
  # Check that all 3 warnings were triggered
  expect_equal(length(warnings), 3)
})

# Setup: Build deterministic dataframe on which to test parameters -------------

# Build a dataframe for classification
d <- data.frame(id = 1:600,
                x = rep(c("true", "false"), times = 300),
                y = rep(c("A", "B", "C"), times = 200),
                z = rep(seq(-1, 1, length.out = 50), times = 12))

# Add a response variable
temp <- ((d$x == "true") - (d$x == "false")) + 
  ((d$y == "A") + 0.5 * (d$y == "B") - 1.5 * (d$y == "C")) +
  1.5 * d$z + 
  c(seq(-1, 1, length.out = 233), seq(1, -1, length.out = 367)) # add noise
  
d$target <- ifelse(temp > 0, "Y", "N")

# Develop a random forest model
p <- SupervisedModelDevelopmentParams$new()
p$df <- d
p$type <- "classification"
p$predictedCol <- "target"
p$grainCol <- "id"
p$cores <- 1
capture.output(rf <- RandomForestDevelopment$new(p))
ignoreSpecWarn(capture.output(rf$run()),
               wRegexps = "ROC curve with AUC == 1")

dDeploy <- data.frame(id = 501:503,
                      x = c("true", "false", "false"),
                      y = c("A", "C", "B"),
                      z = c(1, -1, 0))

# Set up development parameters 
p2 <- SupervisedModelDeploymentParams$new()
p2$df <- dDeploy
p2$type <- "classification"
p2$predictedCol <- "target"
p2$grainCol <- "id"
p2$cores <- 1

# Resume Tests: test that the function parameters behave as desired ------------

test_that("output dataframe values are as expected", {
  # Deploy in test to avoid warning
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  
  pvdf <- rfD$getProcessVariablesDf(modifiableVariables = c("x", "y"))
  
  # Check specific values in first row
  expect_equal(pvdf$Modify1TXT[1], "y")
  expect_equal(pvdf$Modify1Current[1], "A")
  expect_equal(pvdf$Modify1AltValue[1], "C")
  
  # Check specific values in second row
  expect_equal(pvdf$Modify2TXT[2], "y")
  expect_equal(pvdf$Modify2Current[2], "C")
  expect_equal(pvdf$Modify2AltValue[2], "C")
  expect_equal(pvdf$Modify1Delta[2], 0)
  
  # Check that the factors are ordered correctly
  expect_true(all(pvdf$Modify1Delta <= pvdf$Modify2Delta))
  
  # Check computation of deltas
  expect_equal(pvdf$Modify1Delta, 
               pvdf$Modify1AltPrediction - pvdf$PredictedProbNBR)
})

test_that("smallerBetter parameter works", {
  # Deploy in test to avoid warning
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  
  pvdf <- rfD$getProcessVariablesDf(modifiableVariables = c("x", "y"), 
                                    smallerBetter = FALSE)  
  
  # Check that deltas are non-negative
  expect_true(all(pvdf$Modify1Delta >= 0))
  
  # Check that the factors are ordered correctly
  expect_true(all(pvdf$Modify1Delta >= pvdf$Modify2Delta))
  expect_true(all(pvdf$Modify2Delta >= pvdf$Modify3Delta))
  
  # Check computation of deltas
  expect_equal(pvdf$Modify1Delta, 
               pvdf$Modify1AltPrediction - pvdf$PredictedProbNBR)
  
  # Check specific values in first row
  expect_equal(pvdf$Modify1TXT[1], "x")
  expect_equal(pvdf$Modify1Current[1], "true")
  expect_equal(pvdf$Modify1AltValue[1], "true")
  expect_equal(pvdf$Modify1Delta[1], 0)
})

test_that("repeatedFactors and numTopFactors parameters work", {
  # Deploy in test to avoid warning
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  
  numberOfFactors <- 5
  pvdf <- rfD$getProcessVariablesDf(modifiableVariables = c("x", "y"),
                                    repeatedFactors = TRUE, 
                                    numTopFactors = numberOfFactors)
  
  # Check the number of columns is correct: 2 + 5 * numTopFactors
  expect_equal(ncol(pvdf), 2 + 5*numberOfFactors)
  
  # Get the variables appearing as modifiable
  variables <- unlist(pvdf[1, c("Modify1TXT",
                                "Modify2TXT", 
                                "Modify3TXT", 
                                "Modify4TXT", 
                                "Modify5TXT")])
  
  # Check that repeated factor occurs
  expect_true(any(table(variables) > 1))
})

test_that("grain column matching works", {
  # Deploy in test to avoid warning
  capture.output(rfD <- RandomForestDeployment$new(p2))
  capture.output(rfD$deploy())
  
  pvdf <- rfD$getProcessVariablesDf(modifiableVariables = c("x", "y"), 
                                    grainColumnIDs = 502:503)
  
  # Check number of rows
  expect_equal(nrow(pvdf), 2)
  
  # Check grain column
  expect_equal(pvdf$id, 502:503)
  
  # Check specific values in first row (second row in deploy data)
  expect_equal(pvdf$Modify2TXT[1], "y")
  expect_equal(pvdf$Modify2Current[1], "C")
  expect_equal(pvdf$Modify2AltValue[1], "C")
  expect_equal(pvdf$Modify1Delta[1], 0)
  
  # Check that ids are sorted, regardless of input order
  pvdf <- rfD$getProcessVariablesDf(modifiableVariables = c("x", "y"), 
                                    grainColumnIDs = c(503, 501))
  
  # Check grain column
  expect_equal(pvdf$id, c(501, 503))
})
