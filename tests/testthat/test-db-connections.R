# Build connection string ---------------------------
context("Checking that build connection string works")

test_that("empty requests a server name", {
  expect_error(build_connection_string(server = 004),
               "provide a quoted server")
})

test_that("trusted connection works", {
  cs <- build_connection_string(server = "localhost")
  expect_equal(class(cs), "character")
  expect_true(stringr::str_detect(cs, "server=\\{localhost\\}"))
  expect_true(stringr::str_detect(cs, "trusted_connection=true"))
})

test_that("uid/pwd works", {
  cs <- build_connection_string(server = "localhost",
                                user_id = "john123",
                                password = "very_secure")
  expect_equal(class(cs), "character")
  expect_true(stringr::str_detect(cs, "uid=john123"))
  expect_true(stringr::str_detect(cs, "pwd=very_secure"))
})

test_that("trusted false throws error with no uid", {
  expect_error(build_connection_string(server = "localhost",
                                       trusted = FALSE,
                                       user_id = "john123"),
               "or provide a user_id")
})

test_that("connection can be made using built string", {
  # skip_on_not_appveyor()
  cs <- build_connection_string(server = "localhost",
                                database = "SAM")
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  expect_equal(class(con), "Microsoft SQL Server")
  DBI::dbDisconnect()
})

# Read Data ----------------------------

# Write Data ---------------------------


#
#
#
# context("Checking that data is selected correctly")
#
#
# # Start with SQLite
# # Grab database file
# sqliteFile <- system.file("extdata",
#                           "unit-test.sqlite",
#                           package = "healthcareai")
#
# query1 = '
# SELECT
# SystolicBPNBR
# FROM [HCRDiabetesClinical] order by PatientEncounterID LIMIT 10'
#
# query2 = '
# select
# LDLNBR
# FROM [HCRDiabetesClinical] order by LDLNBR LIMIT 0'
#
# query3 = '
# SELECT [A1CNBR] FROM [SAM].[dbo].[DiabetesClinicall]'
#
# test_that("SQLite - Returns correct selected data in data frame", {
#   expect_equal(selectData(query = query1,
#                           SQLiteFileName = sqliteFile),
#                data.frame(SystolicBPNBR = c(167,153,170,187,188,
#                                             185,189,149,155,160)))
# })
#
# test_that("SQLite - Returns zero rows msg when zero rows selected", {
#   expect_warning(grepl(selectData(query = query2,
#                                   SQLiteFileName = sqliteFile),
#                  'Zero rows returned from SQL.'))
# })
#
# test_that("SQLite - Returns SQL error message when SQL error", {
#   expect_error(grepl(selectData(query = query3,
#                                 SQLiteFileName = sqliteFile),
#                'Your SQL likely contains an error.'))
# })
#
# test_that("Deprecated argument gives error", {
#   expect_error(grepl(selectData(connectionString = '', query = ''),
#                      'connectionString argument has been deprecated'))
# })

# Start SQL Server tests
# connection.string = '
# driver={SQL Server};
# server=localhost;
# database=SAM;
# trusted_connection=true'
#
# query4 = '
# SELECT TOP 10
#       SystolicBPNBR
# FROM [SAM].[dbo].[HCRDiabetesClinical] order by PatientEncounterID'
#
# query5 = '
# select top 0
#        LDLNBR
# from [SAM].[dbo].[HCRDiabetesClinical] order by LDLNBR'
#
# query6 = '
# SELECT [A1CNBR] FROM [SAM].[dbo].[DiabetesClinicall]'
#
# test_that("SQL Server - Returns correct selected data in data frame", {
#   skip_on_not_appveyor()
#
#   expect_equal(selectData(MSSQLConnectionString = connection.string, query4),
#                data.frame(SystolicBPNBR = c(167,153,170,187,188,
#                                             185,189,149,155,160)))
# })
#
# test_that("SQL Server - Returns zero rows msg when zero rows selected", {
#   skip_on_not_appveyor()
#
#   expect_warning(grepl(selectData(MSSQLConnectionString = connection.string,
#                                   query5),
#                  'Zero rows returned from SQL.'))
# })
#
# test_that("SQL Server - Returns SQL error message when SQL error", {
#   skip_on_not_appveyor()
#
#   expect_error(selectData(MSSQLConnectionString = connection.string, query6),
#                'Your SQL likely contains an error.')
# })
