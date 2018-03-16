context("lints")
test_that("Package is lint free", {
  skip_on_appveyor()
  lintr::expect_lint_free()
})
