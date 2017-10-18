# These tests check that the getTopFactors getter function works correctly
# 
# Tests include:
# 1. Checking that the number of factors to output can be specified
# 2. Checking that the weights are correctly ordered
# 3. Checking that the top factors and weights are stable (always have the same
#    values for a fixed data set and seed)

context("Checking top factors getter works")

set.seed(256)

# Build training data
n = 300
d <- data.frame(id = 1:n,
                x = rnorm(n),
                y = rnorm(n),
                z = rnorm(n),
                w = rnorm(n))
d['response'] <- ifelse(4*d$x - 3*d$y + 2*d$z - d$w + rnorm(n, sd = 1.5) > 0, 
                        "Y", "N")

# Develop and run model
p <- SupervisedModelDevelopmentParams$new()
p$df <- d
p$type <- "classification"
p$predictedCol <- "response"
p$grainCol <- "id"
p$cores <- 1

capture.output(rf <- RandomForestDevelopment$new(p))
capture.output(rf$run())

# New prediction data
dDeploy <- data.frame(id = c(1001, 1002, 1003),
                      x = c(1, 1, 0),
                      y = c(-1, 1, -1),
                      z = c(1, 1, 3),
                      w = c(-1, 1, -10))

# Deploy model
p2 <- SupervisedModelDeploymentParams$new()
p2$df <- dDeploy
p2$type <- "classification"
p2$predictedCol <- "response"
p2$grainCol <- "id"
p$cores <- 1

capture.output(rfD <- RandomForestDeployment$new(p2))
capture.output(rfD$deploy())

###### BEGIN TESTS ######

test_that("The right number of factors are included", {
  factorsDf <- rfD$getTopFactors()
  factorsDf2 <- rfD$getTopFactors(numberOfFactors = 2)
  # check the number of columns
  expect_equal(ncol(factorsDf), 5) # 4 + 1 for grainCol
  expect_equal(ncol(factorsDf2), 3)
})

test_that("Factor weights are ordered", {
  factorsDf <- rfD$getTopFactors(includeWeights = TRUE)
  # Check ordering
  expect_true(all(factorsDf$Factor1Weight >= factorsDf$Factor2Weight))
  expect_true(all(factorsDf$Factor2Weight >= factorsDf$Factor3Weight))
  expect_true(all(factorsDf$Factor3Weight >= factorsDf$Factor4Weight))
})

test_that("Factors and weights are stable", {
  factorsDf <- rfD$getTopFactors(includeWeights = TRUE)
  
  expect_equal(factorsDf$Factor1TXT[1], "x")
  expect_equal(factorsDf$Factor4TXT[2], "y")
  expect_equal(round(factorsDf$Factor2Weight[2], 5), 3.6067)
  expect_equal(round(factorsDf$Factor3Weight[3], 5), 4.59355)
})