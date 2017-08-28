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
                "3 rows were inserted into the SQLite table HCRWriteData")
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
  
  expect_false('HCRWriteData1' %in% DBI::dbListTables(con))
  expect_output(writeData(df = df,
                          SQLiteFileName = sqliteFile,  
                          tableName = 'HCRWriteData1'),
                "3 rows were inserted into the SQLite table HCRWriteData1")

  DBI::dbRemoveTable(con, 'HCRWriteData1')
  DBI::dbDisconnect(con)
})

test_that("SQLite - check that table exists after inserting", {

  df <- data.frame(a = c(1,2,3),
                   b = c(2,4,6),
                   c = c('one','two','three'))
  
  con <- DBI::dbConnect(RSQLite::SQLite(), 
                        dbname = sqliteFile)
  
  expect_false(('HCRWriteData2' %in% DBI::dbListTables(con)))
  
  capture.output(writeData(df = df,
                           SQLiteFileName = sqliteFile,
                           tableName = 'HCRWriteData2'))
  
  expect_true(('HCRWriteData2' %in% DBI::dbListTables(con)))
  
  DBI::dbRemoveTable(con, 'HCRWriteData2')
  DBI::dbDisconnect(con)

})