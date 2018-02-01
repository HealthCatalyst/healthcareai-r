data(mtcars)
test_that("split train test returns a named list with two data frames", {
  sp <- split_train_test(mtcars, am, .8)
  expect_true(is.list(sp))
  expect_s3_class(sp[[1]], "data.frame")
  expect_named(sp)
  expect_equal(c("train", "test"), names(sp))
})

test_that("split train test respects seed", {
  sp1 <- split_train_test(mtcars, am, .8, 123)
  sp2 <- split_train_test(mtcars, am, .8, 123)
  expect_equal(sp1, sp2)
})
