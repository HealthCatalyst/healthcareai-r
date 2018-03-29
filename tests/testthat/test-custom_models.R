context("checking custom models")

test_that("adjust_knn doesn't barf", {
  expect_error(adjust_knn(), NA)
  expect_equal(class(adjust_knn()), class(caret::getModelInfo("kknn")$kknn))
})
