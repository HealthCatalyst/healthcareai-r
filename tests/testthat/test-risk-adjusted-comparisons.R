context("Checking Risk-adjusted Comparisons")

# Can delete these four lines when you set up your SQL connection/querycsvfile <- system.file("extdata", "DiabetesClinical.csv",package = "healthcareai")
csvfile <- system.file("extdata",
                       "HCRDiabetesClinical.csv",
                       package = "healthcareai")

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

dfExp <- data.frame(groupbyList=c('M', 'F'), comparativePerformance=c(-4,4))

test_that("Risk-adjusted comparison runs", {
  expect_equal(riskAdjComp$dfReturn, dfExp)
})
