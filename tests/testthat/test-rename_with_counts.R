context("Checking add_count")

d <- tibble(animal = sample(c("cat", "dog", "mouse", "rabbit"), 20, TRUE),
            other_var = rnorm(20))

# Tests ------------------------------------------------------------------------
test_that("Test normal functionality", {
  expected <- paste0(names(table(d$animal)), " (n = ", table(d$animal), ")")
  actual_tibble <- rename_with_counts(d, animal)
  actual <- all(expected %in% actual_tibble$animal)
  expect_true(actual)
})

test_that("Test stop when not a dataframe", {
  expect_error(rename_with_counts(c(0, 1), animal))
})

test_that("Test numeric unique counts", {
  expect_warning(rename_with_counts(d, other_var))
})
