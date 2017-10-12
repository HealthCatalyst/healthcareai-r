context("Checking that data is selected correctly")


# Start with SQLite
# Grab database file
sqliteFile <- system.file("extdata",
                          "unit-test.sqlite",
                          package = "healthcareai")

query1 = '
SELECT
SystolicBPNBR
FROM [HCRDiabetesClinical] order by PatientEncounterID LIMIT 10'

query2 = '
select
LDLNBR
FROM [HCRDiabetesClinical] order by LDLNBR LIMIT 0'

query3 = '
SELECT [A1CNBR] FROM [SAM].[dbo].[DiabetesClinicall]'

test_that("SQLite - Returns correct selected data in data frame", {
  expect_equal(selectData(query = query1, 
                          SQLiteFileName = sqliteFile),
               data.frame(SystolicBPNBR = c(167,153,170,187,188,
                                            185,189,149,155,160)))
})

test_that("SQLite - Returns zero rows msg when zero rows selected", {
  expect_warning(grepl(selectData(query = query2, 
                                  SQLiteFileName = sqliteFile),
                 'Zero rows returned from SQL.'))
})

test_that("SQLite - Returns SQL error message when SQL error", {
  expect_error(grepl(selectData(query = query3, 
                                SQLiteFileName = sqliteFile),
               'Your SQL likely contains an error.'))
})

test_that("Deprecated argument gives error", {
  expect_error(grepl(selectData(connectionString = '', query = ''),
                     'connectionString argument has been deprecated'))
})

# Start SQL Server tests
connection.string = '
driver={SQL Server};
server=localhost;
database=SAM;
trusted_connection=true'

query4 = '
SELECT TOP 10
      SystolicBPNBR
FROM [SAM].[dbo].[HCRDiabetesClinical] order by PatientEncounterID'

query5 = '
select top 0
       LDLNBR
from [SAM].[dbo].[HCRDiabetesClinical] order by LDLNBR'

query6 = '
SELECT [A1CNBR] FROM [SAM].[dbo].[DiabetesClinicall]'

test_that("SQL Server - Returns correct selected data in data frame", {
  skip_on_not_appveyor()

  expect_equal(selectData(MSSQLConnectionString = connection.string, query4),
               data.frame(SystolicBPNBR = c(167,153,170,187,188,
                                            185,189,149,155,160)))
})

test_that("SQL Server - Returns zero rows msg when zero rows selected", {
  skip_on_not_appveyor()

  expect_warning(grepl(selectData(MSSQLConnectionString = connection.string, 
                                  query5),
                 'Zero rows returned from SQL.'))
})

test_that("SQL Server - Returns SQL error message when SQL error", {
  skip_on_not_appveyor()

  expect_error(selectData(MSSQLConnectionString = connection.string, query6),
               'Your SQL likely contains an error.')
})