context("Checking deploy supervised model")

csvfile <- system.file("extdata", "HREmployeeDeploy.csv", package = "HCRTools")
df <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings = 'NULL')

connection.string <- 'driver={SQL Server};
                      server=localhost;
                      database=SAM;
                      trusted_connection=true'

test_that("rf predicted val (with spec mtry) is the same each time", {

  df$VacationHours <- NULL # This is set as regression prediction col

  o <- DeploySupervisedModel$new(type = 'classification',
                                 df = df,
                                 grain.col = 'GrainID',
                                 test.window.col = 'InTestWindow',
                                 predicted.col = 'SalariedFlag',
                                 impute = TRUE,
                                 use.saved.model = FALSE)

  set.seed(43)
  capture.output(o$deploy(model = 'rf',
                 cores = 1,
                 sqlcnxn = connection.string,
                 dest.schema.table = 'dbo.HCRDeployClassificationBASE',
                 rfmtry = 3))

  expect_equal(as.numeric(o$rf.predictedVALS),0.917870962647079)
})

test_that("lasso predicted val (with spec mtry) is the same each time", {

  df$VacationHours <- NULL # This is set as regression prediction col

  o <- DeploySupervisedModel$new(type = 'classification',
                                 df = df,
                                 grain.col = 'GrainID',
                                 test.window.col = 'InTestWindow',
                                 predicted.col = 'SalariedFlag',
                                 impute = TRUE,
                                 use.saved.model = FALSE)

  set.seed(43)
  capture.output(o$deploy(model = 'lasso',
                          cores = 1,
                          sqlcnxn = connection.string,
                          dest.schema.table = 'dbo.HCRDeployClassificationBASE',
                          rfmtry = 3))

  expect_equal(as.numeric(o$linear.predictedVALS),0.877035858794219)
})

test_that("rf predicted val (w/out spec mtry) is the same each time", {

  df$VacationHours <- NULL # This is set as regression prediction col

  o <- DeploySupervisedModel$new(type = 'classification',
                                 df = df,
                                 grain.col = 'GrainID',
                                 test.window.col = 'InTestWindow',
                                 predicted.col = 'SalariedFlag',
                                 impute = TRUE,
                                 use.saved.model = FALSE)

  set.seed(43)
  capture.output(o$deploy(model = 'rf',
                          cores = 1,
                          sqlcnxn = connection.string,
                          dest.schema.table = 'dbo.HCRDeployClassificationBASE'
                          ))

  expect_equal(as.numeric(o$rf.predictedVALS),0.707460652364422)
})

test_that("lasso predicted val (w/out spec mtry) is the same each time", {

  df$VacationHours <- NULL # This is set as regression prediction col

  o <- DeploySupervisedModel$new(type = 'classification',
                                 df = df,
                                 grain.col = 'GrainID',
                                 test.window.col = 'InTestWindow',
                                 predicted.col = 'SalariedFlag',
                                 impute = TRUE,
                                 use.saved.model = FALSE)

  set.seed(43)
  capture.output(o$deploy(model = 'lasso',
                          cores = 1,
                          sqlcnxn = connection.string,
                          dest.schema.table = 'dbo.HCRDeployClassificationBASE'
  ))

  expect_equal(as.numeric(o$linear.predictedVALS),0.877035858794219)
})



