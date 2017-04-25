context("Checking that data is selected correctly")


# Start with SQLite
# Load csv into SQLite table

csvfile <- system.file("extdata", 
                       "HCRDiabetesClinical.csv", 
                       package = "healthcareai")

# Replace csvfile with 'path/file'
df <- read.csv(file = csvfile, 
               header = TRUE, 
               na.strings = c("NULL", "NA", ""))

con = DBI::dbConnect(RSQLite::SQLite(), dbname = "unit-test.sqlite")

DBI::dbSendQuery(conn = con,
                 "CREATE TABLE HCRDiabetesClinical
                 (PatientEncounterID INTEGER,
                 PatientID INTEGER,
                 SystolicBPNBR INTEGER,
                 LDLNBR INTEGER,
                 A1CNBR REAL,
                 GenderFLG TEXT,
                 ThirtyDayReadmitFLG TEXT,
                 InTestWindowFLG TEXT,
                 PRIMARY KEY (PatientEncounterID))"
)

DBI::dbWriteTable(conn = con, 
                  name = "HCRDiabetesClinical", 
                  df,
                  overwrite = T,
                  append = F, 
                  row.names = F)

query0 = '
SELECT
SystolicBPNBR
FROM [HCRDiabetesClinical] order by PatientEncounterID LIMIT 10'

test_that("SQLite - Returns correct selected data in data frame", {
  expect_equal(selectData(query = query0, dbType = 'SQLite'),
               data.frame(SystolicBPNBR = c(167,153,170,187,188,
                                            185,189,149,155,160)))
})

queryDrop <- 'DROP TABLE HCRDiabetesClinical'

df <- capture.output(selectData(query = queryDrop, dbType = 'SQLite'))

DBI::dbDisconnect(con)

# Start SQL Server tests

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


test_that("SQL Server - Returns correct selected data in data frame", {
  skip_on_travis()
  skip_on_cran()
  expect_equal(selectData(connection.string, query1),
               data.frame(SystolicBPNBR = c(167,153,170,187,188,
                                            185,189,149,155,160)))
})

test_that("SQL Server - Returns too few rows message when 0 rows edge case", {
  skip_on_travis()
  skip_on_cran()
  expect_equal(capture.output(selectData(connection.string,query2)),
               capture.output(cbind(cat('Zero rows returned from SQL. Adjust your query to return more data!'),data.frame(LDLNBR = character(),stringsAsFactors = FALSE))))
})

test_that("SQL Server - Returns SQL error message when SQL error", {
  skip_on_travis()
  skip_on_cran()
  expect_error(capture.output(selectData(connection.string,query3)),
               'Your SQL contains an error.')
})

