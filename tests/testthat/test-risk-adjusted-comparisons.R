context("Checking Risk-adjusted Comparisons")

connection.string = "
driver={SQL Server};
server=localhost;
database=AdventureWorks2012;
trusted_connection=true
"

query = "
SELECT
[OrganizationLevel]
,[MaritalStatus]
,[Gender]
,IIF([SalariedFlag]=0,'N','Y') AS SalariedFlag
,[VacationHours]
,[SickLeaveHours]
FROM [AdventureWorks2012].[HumanResources].[Employee]
WHERE OrganizationLevel <> 0
"

df <- selectData(connection.string, query)

p <- SupervisedModelParameters$new()
p$df = df
p$groupCol = 'OrganizationLevel'
p$impute = TRUE
p$predictedCol = 'SalariedFlag'
p$debug = FALSE
p$cores = 1

set.seed(5)
riskAdjComp <- RiskAdjustedComparisons$new(p)
capture.output(riskAdjComp$run())

test_that("Risk-adjusted comparison is as expected for group 1", {

  expect_identical(riskAdjComp$dfReturn[1,'comparative.performance'], 31.5)
})

test_that("Risk-adjusted comparison is as expected for group 2", {

  expect_identical(riskAdjComp$dfReturn[2,'comparative.performance'], 20.5)
})

test_that("Risk-adjusted comparison is as expected for group 3", {

  expect_identical(riskAdjComp$dfReturn[3,'comparative.performance'], -14.5)
})

test_that("Risk-adjusted comparison is as expected for group 4", {

  expect_identical(riskAdjComp$dfReturn[4,'comparative.performance'], -37.5)
})
