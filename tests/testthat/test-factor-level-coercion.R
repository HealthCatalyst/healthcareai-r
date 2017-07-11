context("Checking factor level coercion is working")

# set seed for reproducibility
set.seed(7)

# build hot dog set
n = 300
df <- data.frame(id = 1:n,
                 length = rnorm(n, mean = 7, sd = 2),
                 diameter = rnorm(n, mean = 2, sd = 0.5), 
                 heat = sample(c("Cold", "Hot"), size = n, replace = T),
                 condiment = sample(c("Ketchup", "Mustard", "Wasabi", "Syrup"), 
                                    size = n, replace = T)
                 )

# give hotdog likeliness score
df['temp'] <- df['length'] - 2*df['diameter'] - 1
df$temp[df['heat'] == "Hot"]  = df$temp[df['heat'] == "Hot"] + 1
df$temp[df['heat'] == "Cold"]  = df$temp[df['heat'] == "Cold"] - 1
df$temp[df['condiment'] == "Ketchup"] <- df$temp[df['condiment'] == "Ketchup"] + 1
df$temp[df['condiment'] == "Mustard"] <- df$temp[df['condiment'] == "Mustard"] + 2
df$temp[df['condiment'] == "Wasabi"] <- df$temp[df['condiment'] == "Wasabi"] - 1
df$temp[df['condiment'] == "Syrup"] <- df$temp[df['condiment'] == "Syrup"] - 4

# assign response variable
df["isHotDog"] <- ifelse(df["temp"] > 0, "Y", "N")
df$temp <- NULL

# swap response variable for 5 percent of data
noise <- sample(1:n, size = round(n/20), replace = F)
df$isHotDog[noise] <- ifelse(df$isHotDog[noise] == "Y", "N", "Y")
df$isHotDog <- as.character(df$isHotDog)

p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "classification"
p$impute <- TRUE
p$grainCol <- "id"
p$predictedCol <- "isHotDog"
p$debug <- FALSE
p$cores <- 1

# Run RandomForest
RandomForest <- RandomForestDevelopment$new(p)
capture.output(RandomForest$run())
# Run Lasso
Lasso <- LassoDevelopment$new(p)
capture.output(Lasso$run())

# reset seed
set.seed(NULL)

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

# Single row data set with NAs
dfDeploy5 <- data.frame(id = 9010,
                        length = 8,
                        diameter = NA, 
                        heat = "Hot",
                        condiment = "NA")

p5 <- SupervisedModelDeploymentParams$new()
p5$type <- "classification"
p5$df <- dfDeploy5
p5$grainCol <- "id"
p5$predictedCol <- "isHotDog"
p5$impute <- TRUE
p5$debug <- F
p5$cores <- 1

#### BEGIN TESTS ####

test_that("Single row predictions work for RF", {
  # Deploy rf on single row
  capture.output(rfD1 <- RandomForestDeployment$new(p1))
  rfD1$deploy()
  
  # Get prediction
  rfOutDf1 <- rfD1$getOutDf()
  
  # Check that dimensions of prediction dataframe are correct
  expect_equal(dim(rfOutDf1)[1], 1)
  expect_equal(dim(rfOutDf1)[2], 8)
})

test_that("Single row predictions work for lasso", {
  # Deploy lasso on single row
  capture.output(lassoD1 <- LassoDeployment$new(p1))
  lassoD1$deploy()
  
  # Get prediction
  lassoOutDf1 <- lassoD1$getOutDf()
  
  # Check that dimensions of prediction dataframe are correct
  expect_equal(dim(lassoOutDf1)[1], 1)
  expect_equal(dim(lassoOutDf1)[2], 8)
})

test_that("RF predictions are independent of each other", {
  # Deploy rf on single row
  capture.output(rfD1 <- RandomForestDeployment$new(p1))
  rfD1$deploy()
  
  rfOutDf1 <- rfD1$getOutDf()
  
  # Deploy rf on 2 rows, one of which is identical to the one above
  capture.output(rfD2 <- RandomForestDeployment$new(p2))
  rfD2$deploy()
  
  rfOutDf2 <- rfD2$getOutDf()
  
  # Check that the predictions are the same
  expect_equal(rfOutDf1[1, ]$PredictedProbNBR, rfOutDf2[1, ]$PredictedProbNBR)
})

test_that("Lasso predictions are independent of each other", {
  # Deploy lasso on single row
  capture.output(lassoD1 <- LassoDeployment$new(p1))
  lassoD1$deploy()
  
  lassoOutDf1 <- lassoD1$getOutDf()
  
  # Deploy lasso on 2 rows, one of which is identical to the one above
  capture.output(lassoD2 <- LassoDeployment$new(p2))
  lassoD2$deploy()
  
  lassoOutDf2 <- lassoD2$getOutDf()
  
  # Check that the predictions are the same
  expect_equal(lassoOutDf1[1, ]$PredictedProbNBR, lassoOutDf2[1, ]$PredictedProbNBR)
})

test_that("Extra factors are imputed correctly for rf (1 column)", {
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

test_that("Extra factors are imputed correctly for lasso  (1 column)", {
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

test_that("Extra factors are imputed correctly for rf (2 columns)", {
  # Deploy on 3 rows, which should yield the same predictions
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

test_that("Extra factors are imputed correctly for lasso (2 columns)", {
  # Deploy on 3 rows, which should yield the same predictions
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

# TODO: make test for single row predictions when data contains NAs
# Currently, imputation is done using the deployset so will fail if there is 
# only one row

