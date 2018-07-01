if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("healthcareai", filter = "^[(s-y)|(S-Y)]")
}
