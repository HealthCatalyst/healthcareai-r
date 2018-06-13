library(testthat)
library(healthcareai)
Sys.setenv("R_TESTS" = "")
test_check("healthcareai", filter = "^[(a-m)|(A-M)]")
