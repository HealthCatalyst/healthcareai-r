# The first set of tests check that the function removeColsWithAllSameValue
# works as desired. The second set of tests check that develop and deploy use
# the same columns (e.g., columns dropped in develop are also dropped in deploy,
# columns added in deploy are not used).
#
# These tests include:
# 1. Testing that columns with zero variance, which are dropped in develop are
#    also dropped in deploy
# 2. Check that an error is raised if variables which were used in develop are
#    missing in deploy
# 3. Check that new columns added in deploy are dropped

context("Checking that columns are removed when every row in the column has the same value")

test_that("Data frame outputted is same as data frame inputted when every column has rows with different values", {
  df1 = data.frame(a=c(1,2,3),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,1,2))
  expect_identical(removeColsWithAllSameValue(df1),df1)
})

test_that("Remove one column", {
  df1 = data.frame(a=c(1,1,1),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,1,NA))
  resdf = data.frame(b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE))
  expect_identical(removeColsWithAllSameValue(df1),resdf)

  df2 = data.frame(a=c(1,2,1),b=c('Y','Y','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,1,2))
  resdf = data.frame(a=c(1,2,1),c=c(FALSE,TRUE,FALSE),d=c(NA,1,2))
  expect_identical(removeColsWithAllSameValue(df2),resdf)

  df3 = data.frame(a=c(1,2,1),b=c('Y','N','Y'),c=c(FALSE,FALSE,FALSE),d=c(NA,1,2))
  resdf = data.frame(a=c(1,2,1),b=c('Y','N','Y'),d=c(NA,1,2))
  expect_identical(removeColsWithAllSameValue(df3),resdf)

  df4 = data.frame(a=c(1,2,1),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,NA,NA))
  resdf = data.frame(a=c(1,2,1),b=c('Y','N','Y'),c=c(FALSE,TRUE,FALSE))
  expect_identical(removeColsWithAllSameValue(df4),resdf)
})

test_that("Remove multiple columns", {
  df1 = data.frame(a=c(1,2,1),b=c('Y','Y','Y'),c=c(FALSE,TRUE,FALSE),d=c(NA,NA,NA))
  resdf = data.frame(a=c(1,2,1),c=c(FALSE,TRUE,FALSE))
  expect_identical(removeColsWithAllSameValue(df1),resdf)
})

test_that("Remove all but one columns", {
  df1 = data.frame(a=c(1,2,1),b=c('Y','Y','Y'),c=c(FALSE,FALSE,FALSE),d=c(NA,NA,NA))
  resdf = data.frame(a=c(1,2,1))
  expect_identical(removeColsWithAllSameValue(df1),resdf)
})

test_that("All columns are removed when all columns have the same value in every row", {
  df1 = data.frame(a=c(1,1,1),b=c('Y','Y','Y'),c=c(FALSE,FALSE,FALSE),d=c(NA,NA,NA))
  expect_output(removeColsWithAllSameValue(df1),
                "All columns were removed.")
})


############# SETUP FOR DROPPING COLUMNS IN DEPLOY

# This function builds a toy dataframe suitable for regression, binary
# classification or multiclass classification
getDf = function(type) {
  set.seed(314)
  n = 200
  d <- data.frame(id = 1:n,
                  x = rnorm(n),
                  y = rnorm(n),
                  a = sample(c("A", "B", "C"), size = n, replace = T))
  temp <- d$x - 2*d$y + 2*(d$a == "A") - (d$a == "B") + rnorm(n)
  if (type == "regression") {
    d["response"] <- temp
  } else if (type == "classification") {
    d["response"] <- ifelse(temp > 0, "Y", "N")
    d$response <- as.factor(d$response)
  } else if (type == "multiclass") {
    d["response"] <- ifelse(temp > 1, 
                            "high",
                            ifelse(temp < -1, "low", "middle"))
  } else {
    stop("type must be one of regression, classification, or multiclass")
  }
  return(d)
}

# This function returns a list of development and deployment parameters
# which can be used with the dataframe built using the function above.
getClassificationParams = function(dfDevelop, dfDeploy, type) {
  p1 <- SupervisedModelDevelopmentParams$new()
  p1$df <- dfDevelop
  p1$type <- type
  p1$predictedCol <- "response"
  p1$grainCol <- "id"
  p1$impute <- TRUE
  p1$cores <- 1
  p1$debug <- FALSE
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$df <- dfDeploy
  p2$type <- type
  p2$predictedCol <- "response"
  p2$grainCol <- "id"
  p2$impute <- TRUE
  p2$cores <- 1
  p2$debug <- FALSE
  
  p <- list()
  p$pDevelop <- p1
  p$pDeploy <- p2
  return(p)
}

########## RESUME TESTS

test_that("Numeric Column with no variance is dropped in RF deploy", {
  # Set up development/deployment parameters
  d <- getDf("classification")
  # Add numeric column with no variance
  d$z <- -0.5
  devSet <- d[1:190, ]
  depSet <- d[191:200, ]
  #Add variance in deploy
  depSet$z <- rnorm(10)
  params <- getClassificationParams(devSet, depSet, "classification")
  p1 <- params$pDevelop
  p2 <- params$pDeploy
  
  # Develop and Deploy Model
  rf <- RandomForestDevelopment$new(p1)
  capture.output(rf$run())
  capture.output(rfD <- RandomForestDeployment$new(p2))
  rfD$deploy()
  
  # Check that numeric column has been dropped
  expect_true(!("z" %in% names(rfD$params$df)))
})

test_that("Factor column with no variance is dropped in RF deploy", {
  # Set up development/deployment parameters
  d <- getDf("classification")
  # Add factor column with no variance
  d$b <- factor("banana", levels = c("alligator", "banana", "potato"))
  devSet <- d[1:190, ]
  depSet <- d[191:200, ]
  #Add variance in deploy
  depSet$b <- sample(c("alligator", "banana", "potato"), size = 10, replace = T)
  params <- getClassificationParams(devSet, depSet, "classification")
  p1 <- params$pDevelop
  p2 <- params$pDeploy
  
  # Develop and Deploy Model
  rf <- RandomForestDevelopment$new(p1)
  capture.output(rf$run())
  capture.output(rfD <- RandomForestDeployment$new(p2))
  rfD$deploy()
  
  # Check that numeric column has been dropped
  expect_true(!("b" %in% names(rfD$params$df)))
})

test_that("Columns with no variance are dropped in XGBoost deploy", {
  # Set up development/deployment parameters
  d <- getDf("multiclass")
  # Add numeric and factor columns with no variance
  d$z <- -0.5
  d$b <- factor("banana", levels = c("alligator", "banana", "potato"))
  devSet <- d[1:190, ]
  depSet <- d[191:200, ]
  #Add variance in deploy
  depSet$z <- rnorm(10)
  depSet$b <- sample(c("alligator", "banana", "potato"), size = 10, replace = T)
  params <- getClassificationParams(devSet, depSet, "multiclass")
  p1 <- params$pDevelop
  p2 <- params$pDeploy
  
  # Develop and Deploy Model
  capture.output(xgb <- XGBoostDevelopment$new(p1))
  capture.output(xgb$run())
  capture.output(xgbD <- XGBoostDeployment$new(p2))
  capture.output(xgbD$deploy())
  
  # Check that numeric column has been dropped
  expect_true(!("z" %in% names(xgbD$params$df)))
  expect_true(!("b" %in% names(xgbD$params$df)))
})

test_that("Error is raised when columns are missing", {
  # Set up development/deployment parameters
  d <- getDf("classification")
  devSet <- d[1:190, ]
  depSet <- d[191:200, ]
  # Drop column in deploy
  depSet$x <- NULL
  params <- getClassificationParams(devSet, depSet, "classification")
  p1 <- params$pDevelop
  p2 <- params$pDeploy
  
  # Develop and Deploy Model
  rf <- RandomForestDevelopment$new(p1)
  capture.output(rf$run())
  expect_error(capture.output(rfD <- RandomForestDeployment$new(p2)), 
               paste0("Some columns used to develop the model are missing\n",
                      "Missing columns: x"))
  
  # Drop second column in deploy
  depSet$a <- NULL
  p2$df <- depSet
  
  # Develop and Deploy Model
  expect_error(capture.output(rfD <- RandomForestDeployment$new(p2)), 
               paste0("Some columns used to develop the model are missing\n",
                      "Missing columns: x a"))
})

test_that("Extra columns in deploy are dropped", {
  # Set up development/deployment parameters
  d <- getDf("classification")
  devSet <- d[1:190, ]
  depSet <- d[191:200, ]
  # Add columns in deploy
  depSet$z <- rnorm(10)
  depSet$b <- sample(c("alligator", "banana", "potato"), size = 10, replace = T)
  params <- getClassificationParams(devSet, depSet, "classification")
  p1 <- params$pDevelop
  p2 <- params$pDeploy
  
  # Develop and Deploy Model
  rf <- RandomForestDevelopment$new(p1)
  capture.output(rf$run())
  capture.output(rfD <- RandomForestDeployment$new(p2))
  rfD$deploy()
  
  # Check that extra columns have been dropped
  expect_true(!("z" %in% names(rfD$params$df)))
  expect_true(!("b" %in% names(rfD$params$df)))
})
