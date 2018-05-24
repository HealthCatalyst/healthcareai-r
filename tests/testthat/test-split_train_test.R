context("test split_train_test.R")

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

test_that("split train test grouping", {
  owner <- c(
    "a",
    "a",
    "b",
    "b",
    "c",
    "c",
    "d",
    "d",
    "e",
    "e",
    "f",
    "f",
    "g",
    "g",
    "h",
    "h",
    "i",
    "i",
    "j",
    "j",
    "k",
    "k",
    "l",
    "l",
    "m",
    "m",
    "n",
    "n",
    "o",
    "o",
    "p",
    "p"
  )
  mtcars <- cbind(mtcars, owner)
  sp1 <- split_train_test(mtcars, mpg, .75, 123, owner)
  result <- length(intersect(sp1[[1]]$owner, sp1[[2]]$owner)) == 0
  expect_true(result)
})
