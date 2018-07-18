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

test_that("group_strat_split - grouping functionality", {
  owner <- as.factor(rep(letters[1:16], each = 2))
  mtcars <- cbind(mtcars, owner)
  sp <- group_strat_split(mtcars, rlang::quo(am), .75, rlang::quo(owner), dplyr::first)
  result <- length(intersect(sp[[1]]$owner, sp[[2]]$owner)) == 0
  expect_true(result)
})

test_that("group_strat_split - grouping aggreg default parameter", {
  owner <- as.factor(rep(letters[1:16], each = 2))
  mtcars <- cbind(mtcars, owner)
  sp <- group_strat_split(mtcars, rlang::quo(am), .75, rlang::quo(owner))
  result <- length(intersect(sp[[1]]$owner, sp[[2]]$owner)) == 0
  expect_true(result)
})

test_that("group_strat_split - grouping stratified split semi reserved", {
  owner <- as.factor(rep(letters[1:16], each = 2))
  mtcars <- cbind(mtcars, owner)
  sp <- group_strat_split(mtcars, rlang::quo(mpg), .75, rlang::quo(owner), mean)
  result <- (mean(sp[[1]]$mpg) - mean(sp[[2]]$mpg)) < 2
  expect_true(result)
})

test_that("split_train_test - grouping functionality", {
  owner <- as.factor(rep(letters[1:16], each = 2))
  mtcars <- cbind(mtcars, owner)
  sp <- split_train_test(mtcars, am, .75, grouping_col = owner, aggreg_func = dplyr::first)
  result <- length(intersect(sp[[1]]$owner, sp[[2]]$owner)) == 0
  expect_true(result)
})

test_that("split_train_test - grouping aggreg default parameter", {
  owner <- as.factor(rep(letters[1:16], each = 2))
  mtcars <- cbind(mtcars, owner)
  sp <- split_train_test(mtcars, am, .75, grouping_col = owner)
  result <- length(intersect(sp[[1]]$owner, sp[[2]]$owner)) == 0
  expect_true(result)
})

test_that("split_train_test - stratified split semi reserved", {
  owner <- as.factor(rep(letters[1:16], each = 2))
  mtcars <- cbind(mtcars, owner)
  sp <- split_train_test(mtcars, mpg, .75, 123, owner, mean)
  result <- (mean(sp[[1]]$mpg) - mean(sp[[2]]$mpg)) < 2
  expect_true(result)
})
