context("lints")
test_that("Package is lint free", {
  skip_on_appveyor()
  lintr::expect_lint_free(
    # Anything changed here must be changed in .lintr too.
    linters = lintr::with_defaults(camel_case_linter = NULL,
                                   line_length_linter = NULL,
                                   object_usage_linter = NULL)
  )
})
