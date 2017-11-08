context("lints")
test_that("Package is lint free", {
  skip_on_appveyor()
  skip_on_travis()
  lintr::expect_lint_free()
})
