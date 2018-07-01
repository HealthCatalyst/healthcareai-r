if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  library(testthat)
  library(healthcareai)
  Sys.setenv("R_TESTS" = "")
  test_check("healthcareai", filter = "^[(j-r)|(J-R)]")
}
