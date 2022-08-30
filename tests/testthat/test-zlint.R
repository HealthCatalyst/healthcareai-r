# context("lints")
# test_that("Package is lint free", {
#   skip_on_appveyor()
#   linters <- lintr::with_defaults(camel_case_linter = NULL,
#                                  line_length_linter = NULL,
#                                  object_usage_linter = NULL,
#                                  cyclocomp_linter = NULL,
#                                  object_name_linter = NULL,
#                                  object_length_linter = lintr::object_length_linter(length = 45L))
#
#   # Anything changed here must be changed in .lintr too.
#   lintr::expect_lint_free(path = getwd(),
#                           relative_path = FALSE,
#                           linters = linters)
# })
