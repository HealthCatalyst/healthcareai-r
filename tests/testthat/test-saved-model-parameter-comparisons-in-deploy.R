# These tests verify that certain parameters of a saved model such as the model
# type (classification, etc.) are compared to the deployment parameters
# before deployment

context("Checking that saved models are vetted in deployment")

test_that("Classification models can only be used for classification", {
  # Build a toy dataframe for classification
  n <- 100
  d <- data.frame(id = 1:n,
                  x = rnorm(n),
                  y = rnorm(n),
                  z = rnorm(n))
  d$response <- ifelse(d$x + d$y + d$z  + rnorm(n) > 0, "Y", "N")
  
  # Set up development parameters
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- d
  p$type <- "classification"
  p$predictedCol <- "response"
  p$grainCol <- "id"
  p$cores <- 1
  
  # Develop a classification model
  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())
  
  # Set up deployment parameters
  p2 <- SupervisedModelDeploymentParams$new()
  p2$df <- d
  p2$type <- "regression" # param is incorrectly set to regression
  p2$predictedCol <- "response"
  p2$grainCol <- "id"
  p2$cores <- 1
  
  expect_error(capture.output(rfD <- RandomForestDeployment$new(p2)),
               paste0("The saved model you have loaded is a classification ",
                      "model, but you are trying to deploy a regression model"))
  
  p2$type <- "multiclass" # param is incorrectly set to multiclass
  
  expect_error(capture.output(rfD <- RandomForestDeployment$new(p2)),
               paste0("The saved model you have loaded is a classification ",
                      "model, but you are trying to deploy a multiclass model"))
})

test_that("Regression models can only be used for regression", {
  # Build a toy dataframe for regression
  n <- 100
  d <- data.frame(id = 1:n,
                  x = rnorm(n),
                  y = rnorm(n),
                  z = rnorm(n))
  d$response <- d$x + d$y + d$z  + rnorm(n)
  
  # Set up development parameters
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- d
  p$type <- "regression"
  p$predictedCol <- "response"
  p$grainCol <- "id"
  p$cores <- 1
  
  # Develop a regression model
  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())
  
  # Set up deployment parameters
  p2 <- SupervisedModelDeploymentParams$new()
  p2$df <- d
  p2$type <- "classification" # param is incorrectly set to classification
  p2$predictedCol <- "response"
  p2$grainCol <- "id"
  p2$cores <- 1
  
  expect_error(capture.output(rfD <- RandomForestDeployment$new(p2)),
               paste0("The saved model you have loaded is a regression ",
                      "model, but you are trying to deploy a classification ",
                      "model"))
  
  p2$type <- "multiclass" # param is incorrectly set to multiclass
  
  expect_error(capture.output(rfD <- RandomForestDeployment$new(p2)),
               paste0("The saved model you have loaded is a regression ",
                      "model, but you are trying to deploy a multiclass model"))
})

test_that("Warning is thrown if predicted columns do not match", {
  # Build toy dataframe with "response" as the predicted column
  n <- 100
  d <- data.frame(id = 1:n,
                  x = rnorm(n),
                  y = rnorm(n),
                  z = rnorm(n))
  d$response <- ifelse(d$x + d$y + d$z  + rnorm(n) > 0, "Y", "N")

  # Set up development parameters and develop model
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- d
  p$type <- "classification"
  p$predictedCol <- "response" # lowercase predicted column
  p$grainCol <- "id"
  p$cores <- 1
  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())

  # Set up deployment parameters
  p2 <- SupervisedModelDeploymentParams$new()
  p2$df <- d
  p2$type <- "classification"
  p2$predictedCol <- "respons" # misspelled predicted column
  p2$grainCol <- "id"
  p2$cores <- 1

  expect_warning(capture.output(rfD <- RandomForestDeployment$new(p2)),
                 regexp = paste0("The name of the predicted column in the ",
                        "saved model differs from the name of the predicted ",
                        "column that you have set.",
                        "\n- Old predicted column: response",
                        "\n- New predicted column: respons"))
})
