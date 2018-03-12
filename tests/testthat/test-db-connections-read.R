# Read from Database ---------------------------
context("Checking that write to database works")
library(dplyr)

cs <- build_connection_string(server = "localhost",
                                  database = "testSAM")


test_that("con is a connection", {
  expect_error(res <- db_read("fake_con",
                              "SELECT * FROM testSAM.dbo.hcai_unit_tests"),
               "SQL Server")
})

test_that("data reads using sql string", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  res <- db_read(con, "SELECT * FROM testSAM.dbo.hcai_unit_tests")
  exp <- tibble::tibble(id = 1:3,
                        word_of_day = c("bagel", "box", "toaster"))
  expect_equal(res, exp)
  DBI::dbDisconnect()
})

test_that("data reads special schema using sql string", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  res <- db_read(con, "SELECT * FROM testSAM.test_schema.hcai_unit_tests")
  exp <- tibble::tibble(id = 4:6,
                        word_of_day = c("lentil", "automobile", "towel"))
  expect_equal(res, exp)
  DBI::dbDisconnect()
})

test_that("data reads a pointer if not collected", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  pt <- db_read(con,
                 query = "SELECT * FROM testSAM.dbo.hcai_unit_tests",
                 pull_into_memory = FALSE)
  res <- pt %>% collect()
  exp <- tibble::tibble(id = 1:3,
                        word_of_day = c("bagel", "box", "toaster"))
  expect_equal(class(pt)[1], "tbl_dbi")
  expect_equal(res, exp)
  DBI::dbDisconnect()
})

test_that("data reads a pointer from special schema if not collected", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  pt <- db_read(con,
                query = "SELECT * FROM testSAM.test_schema.hcai_unit_tests",
                pull_into_memory = FALSE)
  res <- pt %>% collect()
  exp <- tibble::tibble(id = 4:6,
                        word_of_day = c("lentil", "automobile", "towel"))
  expect_equal(class(pt)[1], "tbl_dbi")
  expect_equal(res, exp)
  DBI::dbDisconnect()
})
