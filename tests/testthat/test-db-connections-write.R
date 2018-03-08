# Write to Database ---------------------------
context("Checking that write to database works")
library(dplyr)

my_con <- build_connection_string(server = "localhost",
                                  database = "testSAM")

test_that("not dataframe throws error", {
  skip_on_not_appveyor()
  rcon <- RODBC::odbcDriverConnect(my_con)
  expect_error(db_write("hi", rcon, "hcai_unit_tests"),
               "be a data frame")
  RODBC::odbcClose(rcon)
})

test_that("not RODBC connection throws error", {
  skip_on_not_appveyor()
  d <- data.frame(id = 11, word_of_day = "force")
  expect_error(db_write(d, "string", "hcai_unit_tests"),
               "RODBC")
})

test_that("connection without database throws error", {
  skip_on_not_appveyor()
  d <- data.frame(id = 12, word_of_day = "sprinkler")
  my_con <- build_connection_string(server = "localhost")
  rcon <- RODBC::odbcDriverConnect(my_con)
  expect_error(db_write(d, rcon, "hcai_unit_tests"),
               "database")
  RODBC::odbcClose(rcon)
})
#
# test_that("data appends to default schema", {
#   skip_on_not_appveyor()
#   d <- data.frame(id = 13, word_of_day = "help")
#   rcon <- RODBC::odbcDriverConnect(my_con)
#   out <- db_write(d, rcon, "hcai_unit_tests")
#   expect_equal(out, "1 rows successfully appended.")
#   RODBC::odbcClose(rcon)
# })
#
# test_that("data overwrites to default schema", {
#   skip_on_not_appveyor()
#   d <- data.frame(id = 14, word_of_day = "pickle")
#   rcon <- RODBC::odbcDriverConnect(my_con)
#   out <- db_write(d, rcon, "hcai_unit_tests", overwrite = TRUE)
#   expect_equal(out, "1 rows successfully written.")
#   RODBC::odbcClose(rcon)
# })
#
# test_that("data appends to specified schema", {
#   skip_on_not_appveyor()
#   d <- data.frame(id = 15, word_of_day = "trombone")
#   rcon <- RODBC::odbcDriverConnect(my_con)
#   out <- db_write(d, rcon,
#                   table_name = "hcai_unit_tests",
#                   schema = "test_schema")
#   expect_equal(out, "1 rows successfully appended.")
#   RODBC::odbcClose(rcon)
# })
#
# test_that("data overwrites to specified schema", {
#   skip_on_not_appveyor()
#   d <- data.frame(id = 16, word_of_day = "key")
#   rcon <- RODBC::odbcDriverConnect(my_con)
#   out <- db_write(d, rcon,
#                   table_name = "hcai_unit_tests",
#                   schema = "test_schema",
#                   overwrite = TRUE)
#   expect_equal(out, "1 rows successfully written.")
#   RODBC::odbcClose(rcon)
# })
#
# test_that("fails elegantly when table doesn't exist", {
#   skip_on_not_appveyor()
#   d <- data.frame(id = 17, word_of_day = "blue")
#   rcon <- RODBC::odbcDriverConnect(my_con)
#   expect_error(db_write(d, rcon,
#                   table_name = "junk"),
#                "doesn't exist")
#   RODBC::odbcClose(rcon)
# })
#
# test_that("fails elegantly when schema doesn't exist", {
#   skip_on_not_appveyor()
#   d <- data.frame(id = 18, word_of_day = "noodle")
#   rcon <- RODBC::odbcDriverConnect(my_con)
#   expect_error(db_write(d, rcon,
#                         table_name = "hcai_unit_tests",
#                         schema = "junk"),
#                "doesn't exist")
#   RODBC::odbcClose(rcon)
# })

