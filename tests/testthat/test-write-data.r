context("Checking that data is written to a table correctly")

# Start with SQLite
# Grab database file
sqliteFile <- system.file("extdata",
                          "unit-test.sqlite",
                          package = "healthcareai")

test_that("SQLite - inserts df successfully into table", {
  df <- data.frame(a = c(1,2,3),
                   b = c(2,4,6),
                   c = c('one','two','three'))
  
  expect_output(writeData(df = df,
                          SQLiteFileName = sqliteFile,  
                          tableName = 'HCRWriteData'),
               "3 rows were inserted into the SQL Server HCRWriteData table.")
})

test_that("SQLite - fails to insert if df has extra d column", {
  df <- data.frame(a = c(1,2,3),
                   b = c(2,4,6),
                   c = c('one','two','three'),
                   d = c(1,2,3))
  
  expect_error(writeData(df = df,
                         SQLiteFileName = sqliteFile,  
                         tableName = 'HCRWriteData'),
                "Columns d not found")
})

test_that("SQLite - insert df into table that didn't exist", {

  df <- data.frame(a = c(1,2,3),
                   b = c(2,4,6),
                   c = c('one','two','three'),
                   d = c(1,2,3))
  
  con <- DBI::dbConnect(RSQLite::SQLite(), 
                        dbname = sqliteFile)
  
  expect_false('HCRWriteDataa' %in% DBI::dbListTables(con))
  expect_output(writeData(df = df,
                          SQLiteFileName = sqliteFile,  
                          tableName = 'HCRWriteDataa'),
                "3 rows were inserted into the SQL Server HCRWriteDataa table.")

  DBI::dbRemoveTable(con, 'HCRWriteDataa')
})

test_that("SQLite - check that table exists after inserting", {

  df <- data.frame(a = c(1,2,3),
                   b = c(2,4,6),
                   c = c('one','two','three'))
  
  con <- DBI::dbConnect(RSQLite::SQLite(), 
                        dbname = sqliteFile)
  
  expect_false(('HCRWriteDatat' %in% DBI::dbListTables(con)))
  
  capture.output(writeData(df = df,
                           SQLiteFileName = sqliteFile,
                           tableName = 'HCRWriteDatat'))
  
  expect_true(('HCRWriteDatat' %in% DBI::dbListTables(con)))
  
  DBI::dbRemoveTable(con, 'HCRWriteDatat')
})