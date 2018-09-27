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


# Read from Database ---------------------------
context("Checking that read from database works")
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
  DBI::dbDisconnect(con)
})

test_that("data reads special schema using sql string", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  res <- db_read(con, "SELECT * FROM testSAM.test_schema.hcai_unit_tests")
  exp <- tibble::tibble(id = 4:6,
                        word_of_day = c("lentil", "automobile", "towel"))
  expect_equal(res, exp)
  DBI::dbDisconnect(con)
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
  DBI::dbDisconnect(con)
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
  DBI::dbDisconnect(con)
})

#  Write to database ------------------
cs <- build_connection_string(server = "localhost",
                              database = "testSAM")

test_that("write errors when data or connection is wrong", {
  d <- tibble::tibble(id = 7,
                      word_of_day = c("hello"))
  expect_error(db_write("notdata", "con", "schema", "table", "over"),
               "data frame")
  expect_error(db_write(d, "con", "schema", "table", "over"),
               "Microsoft")
})

test_that("write errors when data or connection is wrong", {
  skip_on_not_appveyor()
  d <- tibble::tibble(id = 7,
                      word_of_day = c("hello"))
  cs2 <- build_connection_string(server = "localhost")
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs2)
  expect_error(db_write(d, con, "schema", "table", "over"),
               "database")
  DBI::dbDisconnect(con)

  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  expect_error(db_write(d, con, "test_schema", "hcai_unit_tests", "over"),
               "overwrite")
  DBI::dbDisconnect(con)
})

test_that("write errors if table/schema doesn't exist", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  d <- tibble::tibble(id = 7,
                      word_of_day = c("hello"))
  expect_error(db_write(d, con, "teriffic_schema", "great_table"),
               "exist")
  expect_error(db_write(d, con, "test_schema", "great_table"),
               "exist")
  expect_error(db_write(d, con, "test_schema", "hcai_unit_tests"),
               NA)
  DBI::dbDisconnect(con)
})

test_that("write column order doesn't matter", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  d <- tibble::tibble(word_of_day = c("hello"),
                      id = 8)
  expect_error(db_write(d, con, "test_schema", "hcai_unit_tests"),
               NA)
  DBI::dbDisconnect(con)
})

test_that("write errors if data types don't match", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  d <- tibble::tibble(id = 3,
                      word_of_day = 7)
  expect_error(db_write(d, con, "test_schema", "hcai_unit_tests"),
               "datatypes")
  d <- tibble::tibble(id = "tib",
                      word_of_day = "hit")
  expect_error(db_write(d, con, "test_schema", "hcai_unit_tests"),
               "datatypes")
  d <- tibble::tibble(id = 5)
  expect_error(db_write(d, con, "test_schema", "hcai_unit_tests"),
               "word")
  d <- tibble::tibble(id = 6,
                      word_of_day = "hit",
                      extra = "whyyy")
  expect_error(db_write(d, con, "test_schema", "hcai_unit_tests"),
               "extra")
  d <- tibble::tibble(word_of_day = "hello",
                      id = 99)
  expect_error(db_write(d, con, "test_schema", "hcai_unit_tests"),
               NA)
  DBI::dbDisconnect(con)
})

test_that("default schema can append and overwrite", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  d <- tibble::tibble(id = 44:46,
                      word_of_day = c("syrup", "sausage", "bar"))
  res <- db_write(d,
                  con,
                  table_name = "hcai_unit_tests")
  expect_equal(res, "3 rows successfully appended to testSAM.dbo.hcai_unit_tests")
  res <- db_read(con, "select * from testSAM.dbo.hcai_unit_tests")
  expect_equal(res$id[nrow(res)], 46)
  res <- db_write(d,
                  con,
                  table_name = "hcai_unit_tests",
                  overwrite = TRUE)
  expect_equal(res, "3 rows successfully written to testSAM.dbo.hcai_unit_tests")
  res <- db_read(con, "select * from testSAM.dbo.hcai_unit_tests")
  expect_equal(res$id[3], 46)
  DBI::dbDisconnect(con)
})

test_that("special schema can append and overwrite", {
  skip_on_not_appveyor()
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  d <- tibble::tibble(id = 4:6,
                        word_of_day = c("lentil", "automobile", "towel"))
  res <- db_write(d,
                  con,
                  table_name = "hcai_unit_tests",
                  schema = "test_schema")
  expect_equal(res, "3 rows successfully appended to testSAM.test_schema.hcai_unit_tests")
  res <- db_read(con, "select * from testSAM.test_schema.hcai_unit_tests")
  expect_equal(res$id[nrow(res)], 6)
  res <- db_write(d,
                  con,
                  table_name = "hcai_unit_tests",
                  schema = "test_schema",
                  overwrite = TRUE)
  expect_equal(res, "3 rows successfully written to testSAM.test_schema.hcai_unit_tests")
  res <- db_read(con, "select * from testSAM.test_schema.hcai_unit_tests")
  expect_equal(res$id[3], 6)
  DBI::dbDisconnect(con)
})

# Utils -----------------------
test_that("SAM utility columns are added.", {
d <- data.frame(a = c(1, 2, NA, NA),
                b = c(100, 300, 200, 150))
d <- add_SAM_utility_cols(d)
expect_equal(ncol(d), 5)
expect_true(all(c("LastLoadDTS", "BindingNM", "BindingID") %in% names(d)))
})
