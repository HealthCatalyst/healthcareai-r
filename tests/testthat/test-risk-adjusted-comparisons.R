context("Checking Risk-adjusted Comparisons")

# Can delete these four lines when you set up your SQL connection/querycsvfile <- system.file("extdata", "DiabetesClinical.csv",package = "HCRTools")
csvfile <- system.file("extdata", "DiabetesClinical.csv", package = "HCRTools")
df <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings = c('NULL', 'NA', ""))


p <- SupervisedModelDevelopmentParams$new()
p$df = df
p$groupCol = 'GenderFLG'
p$impute = TRUE
p$predictedCol = 'ThirtyDayReadmitFLG'
p$debug = FALSE
p$cores = 1

set.seed(5)
riskAdjComp <- RiskAdjustedComparisons$new(p)
capture.output(riskAdjComp$run())

test_that("Risk-adjusted comparison is as expected for group 1", {

  expect_identical(riskAdjComp$dfReturn[1,'comparativePerformance'], -6.5)
})

test_that("Risk-adjusted comparison is as expected for group 2", {

  expect_identical(riskAdjComp$dfReturn[2,'comparativePerformance'], 6.5)
})
