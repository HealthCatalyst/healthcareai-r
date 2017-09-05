context("Checking Risk-adjusted Comparisons")

# Can delete these four lines when you set up your SQL connection/querycsvfile <- system.file("extdata", "DiabetesClinical.csv",package = "healthcareai")
csvfile <- system.file("extdata",
                       "HCRDiabetesClinical.csv",
                       package = "healthcareai")

df <- read.csv(file = csvfile,
                    header = TRUE,
                    na.strings = c('NULL', 'NA', ""))

set.seed(5)
p <- SupervisedModelDevelopmentParams$new()
p$df = df
p$groupCol = 'GenderFLG'
p$impute = TRUE
p$predictedCol = 'ThirtyDayReadmitFLG'
p$debug = FALSE
p$cores = 1

riskAdjComp <- RiskAdjustedComparisons$new(p)
capture.output(riskAdjComp$run())

test_that("Risk-adjusted comparison runs", {
  expect_equal(as.character(riskAdjComp$dfReturn$groupbyList[1]), 'M')
  expect_equal(class(riskAdjComp$dfReturn$comparativePerformance), 'numeric')
})
