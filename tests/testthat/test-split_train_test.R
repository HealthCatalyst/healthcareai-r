context("test split_train_test.R")

data(mtcars)

mtcars_owners <-
  mtcars %>%
  mutate(owner = rep(letters[1:16], each = 2))

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
  train_rows <- group_strat_split(mtcars_owners, rlang::quo(am), .75, rlang::quo(owner))
  sp <- list(train = mtcars_owners[train_rows, ], test = mtcars_owners[-train_rows, ])
  result <- length(intersect(sp[[1]]$owner, sp[[2]]$owner)) == 0
  expect_true(result)
})

test_that("group_strat_split - grouping aggreg default parameter", {
  train_rows <- group_strat_split(mtcars_owners, rlang::quo(am), .75, rlang::quo(owner))
  sp <- list(train = mtcars_owners[train_rows, ], test = mtcars_owners[-train_rows, ])
  result <- length(intersect(sp[[1]]$owner, sp[[2]]$owner)) == 0
  expect_true(result)
})

test_that("group_strat_split - grouping stratified split semi reserved", {
  train_rows <- group_strat_split(mtcars_owners, rlang::quo(mpg), .75, rlang::quo(owner))
  sp <- list(train = mtcars_owners[train_rows, ], test = mtcars_owners[-train_rows, ])
  result <- (mean(sp[[1]]$mpg) - mean(sp[[2]]$mpg)) < 2
  expect_true(result)
})

test_that("split_train_test - grouping functionality", {
  sp <- split_train_test(mtcars_owners, am, .75, grouping_col = owner)
  result <- length(intersect(sp[[1]]$owner, sp[[2]]$owner)) == 0
  expect_true(result)
})

test_that("split_train_test - grouping aggreg default parameter", {
  sp <- split_train_test(mtcars_owners, am, .75, grouping_col = owner)
  result <- length(intersect(sp[[1]]$owner, sp[[2]]$owner)) == 0
  expect_true(result)
})

test_that("split_train_test - stratified split semi reserved", {
  sp <- split_train_test(mtcars_owners, mpg, .75, 123, owner)
  result <- (mean(sp[[1]]$mpg) - mean(sp[[2]]$mpg)) < 2
  expect_true(result)
})
