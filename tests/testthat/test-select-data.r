context("Checking that data is selected correctly")

library(RODBC)
connection.string = '
driver={SQL Server};
server=localhost;
trusted_connection=true'

query1 = '
SELECT TOP 10
      [SystolicBPNBR]
FROM [SAM].[dbo].[HCRDiabetesClinical] order by PatientEncounterID'

query2 = '
select top 30
       LDLNBR
from [SAM].[dbo].[HCRDiabetesClinical] order by LDLNBR'

query3 = '
select top 0
       LDLNBR
from [SAM].[dbo].[HCRDiabetesClinical] order by LDLNBR'

query4 = '
SELECT [A1CNBR] FROM [SAM].[dbo].[DiabetesClinicall]'

test_that("Returns correct selected data in data frame", {
  expect_equal(capture.output(selectData(connection.string,query1)),
               capture.output(rbind(cat('Too few rows returned from SQL: 10 rows returned.Adjust your query to return more data!'),
               data.frame(SystolicBPNBR = c(167,160,126,112,121,
                                            125,133,129,129,145)))))
})

test_that("Returns too few rows message when <200 rows 199 rows edge case", {
  expect_equal(capture.output(selectData(connection.string,query2)),
               capture.output(rbind(cat('Too few rows returned from SQL: 30 rows returned.Adjust your query to return more data!'),
               data.frame(LDLNBR = c(100,100,100,100,100,
                                    100,101,101,101,101,
                                    101,102,102,102,102,
                                    102,102,102,102,102,
                                    103,103,103,103,103,
                                    103,103,104,104,104)))))
})

test_that("Returns too few rows message when 0 rows edge case", {
  expect_equal(capture.output(selectData(connection.string,query3)),
               capture.output(cbind(cat('Too few rows returned from SQL: 0 rows returned.Adjust your query to return more data!'),data.frame(LDLNBR = character(),stringsAsFactors = FALSE))))
})

test_that("Returns SQL error message when SQL error", {
  expect_error(capture.output(selectData(connection.string,query4)),
               'Your SQL contains an error.')
})
