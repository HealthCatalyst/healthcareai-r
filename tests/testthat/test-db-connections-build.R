# Build connection string ---------------------------
context("Checking that build connection string works")
library(dplyr)

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

test_that("connection can be made using built string and DBI", {
  skip_on_not_appveyor()
  cs <- build_connection_string(server = "localhost", database = "testSAM")
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  dd <- DBI::dbGetQuery(con, "select * from testSAM.dbo.hcai_unit_tests")

  expect_equal(names(dd)[1], "id")
  expect_equal(names(dd)[2], "word_of_day")
  expect_equal(dd$word_of_day[1], "bagel")
  expect_equal(class(con)[1], "Microsoft SQL Server")
  DBI::dbDisconnect(con)
})

test_that("connection can be made using built string and dbplyr", {
  skip_on_not_appveyor()
  cs <- build_connection_string(server = "localhost", database = "testSAM")
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  dd <- dplyr::tbl(con,
                   dplyr::sql("select * from testSAM.dbo.hcai_unit_tests")) %>%
    collect()

  expect_equal(names(dd)[1], "id")
  expect_equal(names(dd)[2], "word_of_day")
  expect_equal(dd$word_of_day[1], "bagel")
  expect_equal(class(con)[1], "Microsoft SQL Server")
  DBI::dbDisconnect(con)
})
