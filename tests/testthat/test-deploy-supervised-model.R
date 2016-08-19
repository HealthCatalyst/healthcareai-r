context("Checking deploy supervised model")

csvfile <- system.file("extdata", "HREmployeeDeploy.csv", package = "HCRTools")
totaldf <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings = 'NULL')

test_that("rf predicted val (with spec mtry) is the same each time", {
  set.seed(43)
  o <- DeploySupervisedModel$new(type = 'classification',
                                 df = df,
                                 grain.col = 'GrainID',
                                 test.window.col = 'InTestWindow',
                                 predicted.col = 'SalariedFlag',
                                 impute = TRUE,
                                 debug = FALSE,  # <-- change to TRUE to debug
                                 use.saved.model = FALSE)


  capture.output(o$deploy(model = 'rf',
                 cores = 1,
                 sqlcnxn = connection.string,
                 dest.schema.table = 'dbo.HCRDeployClassificationBASE',
                 debug = FALSE,  # <-- change this to TRUE to debug
                 rfmtry = 2))

  expect_equal(as.numeric(o$rf.predictedVALS),0.707460652364422)
})

test_that("rf predicted val (with spec mtry) is the same each time", {
  set.seed(43)
  o <- DeploySupervisedModel$new(type = 'classification',
                                 df = df,
                                 grain.col = 'GrainID',
                                 test.window.col = 'InTestWindow',
                                 predicted.col = 'SalariedFlag',
                                 impute = TRUE,
                                 debug = FALSE,  # <-- change to TRUE to debug
                                 use.saved.model = FALSE)


  capture.output(o$deploy(model = 'linear',
                          cores = 1,
                          sqlcnxn = connection.string,
                          dest.schema.table = 'dbo.HCRDeployClassificationBASE',
                          debug = FALSE,  # <-- change this to TRUE to debug
                          rfmtry = 2))

  expect_equal(as.numeric(o$linear.predictedVALS),0.877035858794219)
})
