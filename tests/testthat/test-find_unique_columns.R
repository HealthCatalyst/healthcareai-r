library(testthat)
library(healthcareai)

context("Testing find_unique_columns")


# Setup ------------------------------------------------------------------------
d <- data.frame(id_field = c("A","B","C","D"),
                test1_field = c(10,20,30,40),
                test2_field = c(100,200,300,300),
                test3_field = c("A1","B1","B1","D1"),
                test4_field = c("AA","BB","CC","DD"))


# Tests ------------------------------------------------------------------------
test_that("Bad data throws an error", {
  expect_error(find_unique_columns(d))
})

