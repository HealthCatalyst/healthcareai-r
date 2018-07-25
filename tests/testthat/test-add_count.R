context("Checking add_count")

d <- tibble(animal = sample(c("cat", "dog", "mouse", "rabbit"), 20, TRUE),
            other_var = rnorm(20))

# Tests ------------------------------------------------------------------------
test_that("Test normal functionality", {
  expected <-
    d %>%
    count(animal) %>%
    left_join(d, ., by = "animal") %>%
    mutate(animal = paste0(animal, " (n = ", n, ")")) %>%
    select(-n)
  actual <- add_count(d, animal)
  expect_equal(expected, actual)
})

test_that("Test stop when not a dataframe", {
  expect_error(add_count(c(0, 1), animal))
})

test_that("Test numeric unique counts", {
  expected <-
    d %>%
    count(other_var) %>%
    left_join(d, ., by = "other_var") %>%
    mutate(other_var = paste0(other_var, " (n = ", n, ")")) %>%
    select(-n)
  expect_equal(add_count(d, other_var), expected)
})
