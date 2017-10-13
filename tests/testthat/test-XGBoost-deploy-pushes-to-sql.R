context("Checking XGBoost deploy predictions to SQL")

connection.string <- "
driver={SQL Server};
server=localhost;
database=SAM;
trusted_connection=true
"

sqliteFile <- system.file("extdata",
                          "unit-test.sqlite",
                          package = "healthcareai")

csvfile <- system.file("extdata", 
                       "dermatology_multiclass_data.csv", 
                      package = "healthcareai")

df <- read.csv(file = csvfile, header = TRUE, 
             stringsAsFactors = FALSE,
             na.strings = c("NULL", "NA", "", "?"))

dfDeploy <- df[347:366,] 

set.seed(43)
p <- SupervisedModelDevelopmentParams$new()
p$df <- df
p$type <- "multiclass"
p$impute <- TRUE
p$grainCol <- "PatientID"
p$predictedCol <- "target"
p$debug <- FALSE
p$cores <- 1

p$xgb_params <- list("objective" = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "max_depth" = 6, 
                     "eta" = 0.1, 
                     "silent" = 0, 
                     "nthread" = 2)   
# Text of warning that we know will trigger and that we want to suppress
warningText = paste("Each of the following categorical variable levels occurs ",
                    "3 times or fewer:\n-  x6 : Class1\n-  x27 : Class1\n-  ",
                    "x33 : Class1\nConsider grouping",
                    sep = "")

#### BEGIN TESTS ####

test_that("XGBoost deploy pushes values to SQL Server", {
  skip_on_not_appveyor()

  capture.output(ignoreSpecWarn(code = boost <- XGBoostDevelopment$new(p),
                                wRegexps = warningText))
  capture.output(boost$run())
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "multiclass"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientID"
  p2$predictedCol <- "target"
  p2$impute <- TRUE
  p2$debug <- FALSE
  
  capture.output(boostD <- XGBoostDeployment$new(p2))
  capture.output(boostD$deploy())
  capture.output(outDf <- boostD$getOutDf())
  expect_output(writeData(MSSQLConnectionString = connection.string,
                          df = outDf,
                          tableName = 'dermatologyDeployClassificationBASE'),
                "20 rows were inserted into the SQL Server table dermatologyDeployClassificationBASE")
})


test_that("XGBoost deploy pushes values to SQLite", {
  
  capture.output(ignoreSpecWarn(code = boost <- XGBoostDevelopment$new(p),
                                wRegexps = warningText))
  capture.output(suppressWarnings(boost$run()))
  
  p2 <- SupervisedModelDeploymentParams$new()
  p2$type <- "multiclass"
  p2$df <- dfDeploy
  p2$grainCol <- "PatientID"
  p2$predictedCol <- "target"
  p2$impute <- TRUE
  p2$debug <- FALSE
  
  capture.output(boostD <- XGBoostDeployment$new(p2))
  capture.output(boostD$deploy())
  capture.output(outDf <- boostD$getOutDf())
  
  expect_output(writeData(SQLiteFileName = sqliteFile,
                          df = outDf,
                          tableName = 'dermatologyDeployMulticlassBASE'),
                "20 rows were inserted into the SQLite table dermatologyDeployMulticlassBASE")
})