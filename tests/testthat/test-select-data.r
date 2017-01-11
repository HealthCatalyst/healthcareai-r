context("Checking that data is selected correctly")

library(RODBC)
connection.string = '
driver={SQL Server};
server=localhost;
trusted_connection=true'

query1 = '
SELECT TOP 10
      SystolicBPNBR
FROM [SAM].[dbo].[HCRDiabetesClinical] order by PatientEncounterID'

query2 = '
select top 0
       LDLNBR
from [SAM].[dbo].[HCRDiabetesClinical] order by LDLNBR'

query3 = '
SELECT [A1CNBR] FROM [SAM].[dbo].[DiabetesClinicall]'

test_that("Returns correct selected data in data frame", {
  skip_on_travis()
  expect_equal(selectData(connection.string,query1),
               data.frame(SystolicBPNBR = c(167,153,170,187,188,
                                            185,189,149,155,160)))
})

test_that("Returns too few rows message when 0 rows edge case", {
  skip_on_travis()
  expect_equal(capture.output(selectData(connection.string,query2)),
               capture.output(cbind(cat('Too few rows returned from SQL: 0 rows returned.Adjust your query to return more data!'),data.frame(LDLNBR = character(),stringsAsFactors = FALSE))))
})

test_that("Returns SQL error message when SQL error", {
  skip_on_travis()
  expect_error(capture.output(selectData(connection.string,query3)),
               'Your SQL contains an error.')
})

