if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("healthcareai", filter = "^(z|Z)")
}
