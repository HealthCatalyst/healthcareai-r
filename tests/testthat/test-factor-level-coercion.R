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
RandomForest$run()
# Run Lasso
Lasso <- LassoDevelopment$new(p)
Lasso$run()

# reset seed
set.seed(NULL)


test_that("Single row predictions work", {
  #
  dfDeploy1 <- data.frame(id = 9001,
                          length = 6,
                          diameter = 3,
                          heat = "Cold",
                          condiment = "Syrup")
  dfDeploy1
  str(dfDeploy1)
  
  p1 <- SupervisedModelDeploymentParams$new()
  p1$type <- "classification"
  p1$df <- dfDeploy1
  p1$grainCol <- "id"
  p1$predictedCol <- "isHotDog"
  p1$impute <- TRUE
  p1$debug <- T
  p1$cores <- 1
  
  rfD1 <- RandomForestDeployment$new(p1)
  rfD1$deploy()
  
  rfOutDf1 <- rfD1$getOutDf()
  rfOutDf1
  
  # Check that dimensions of prediction dataframe are correct
  expect_equal(dim(rfOutDf1)[1], 1)
  expect_equal(dim(rfOutDf1)[2], 8)
})

test_that("Predictions are independent of each other", {
  #
  dfDeploy2 <- data.frame(id = c(9001, 9002),
                          length = c(6, 2),
                          diameter = c(3, 2), 
                          heat = c("Cold", "Hot"),
                          condiment = c("Syrup", "Mustard"))
  dfDeploy2
  str(dfDeploy2)
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "classification"
  p2$df <- dfDeploy2
  p2$grainCol <- "id"
  p2$predictedCol <- "isHotDog"
  p2$impute <- TRUE
  p2$debug <- F
  p2$cores <- 1
  
  rfD2 <- RandomForestDeployment$new(p2)
  rfD2$deploy()
  
  rfOutDf2 <- rfD2$getOutDf()
  rfOutDf2
  
  lassoD2 <- LassoDeployment$new(p2)
  lassoD2$deploy()
  
  lassoOutDf2 <- lassoD2$getOutDf()
  lassoOutDf2
  
  # Check that the prediction is unchanged when the row is embedded in a 
  # larger data frame
  expect_equal(rfOutDf1[1, ]$PredictedProbNBR, rfOutDf2[1, ]$PredictedProbNBR)
})

test_that("Extra factors", {
  
})

dfDeploy1 <- data.frame(PatientEncounterID = 2001,
                        SystolicBPNBR = round(rnorm(n, mean = 150, sd = 15)),
                        LDLNBR = round(rnorm(n, mean = 180, sd = 30)), 
                        A1CNBR = round(rnorm(n, mean = 5, sd = 2.5), digits = 1),
                        GenderFLG = c('F'))
dfDeploy1