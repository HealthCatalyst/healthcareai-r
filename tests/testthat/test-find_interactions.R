context("Checking test-find_interactions.R")

# Setup ------------------------------------------------------------------------
d <-
  pima_diabetes %>%
  slice(1:100) %>%
  mutate_if(is.character, as.factor)

d_2vars <-
  d %>%
  select(age, diabetes, weight_class)

# Test missing_check -----------------------------------------------------------
test_that("test regression output", {
  expect_message(
    interactions <- find_interactions(select(d, pregnancies, age, weight_class),
                                    outcome = pregnancies),
    "gaussian"
  )
  expect_true(interactions$significance[1] > 0)
})

test_that("test binomial output, converting factor to numeric and logical to numeric", {
  expect_message(
    interactions <- find_interactions(d_2vars, outcome = diabetes),
    "bernoulli"
  )
  expect_true(interactions$significance[1] > 0)
  interactions <- find_interactions(mutate(d_2vars, diabetes = diabetes == "Y"), outcome = diabetes)
  expect_true(interactions$significance[1] > 0)
})

test_that("doesn't crash with missing values", {
  interactions <- find_interactions(d_2vars, outcome = diabetes)
  expect_true(interactions$significance[1] > 0)
})

test_that("random seed works", {

  interactions_1 <- find_interactions(d_2vars, outcome = diabetes)
  interactions_2 <- find_interactions(d_2vars, outcome = diabetes)
  expect_true(round(interactions_1$significance[1], 2) != round(interactions_2$significance[1], 2))

  interactions_1 <- find_interactions(d_2vars, outcome = diabetes, random_seed = 1)
  interactions_2 <- find_interactions(d_2vars, outcome = diabetes, random_seed = 1)
  # There is still some randomness, but it should be small
  expect_equal(round(interactions_1$significance[1], 2),
               round(interactions_2$significance[1], 2))
})

test_that("test verbos", {
  expect_message(
    find_interactions(d_2vars, outcome = diabetes),
    "This will take"
  )

  # No message should be delivered if verbose is silenced.
  mes <- capture_message(
    find_interactions(d_2vars, outcome = diabetes, verbose = FALSE)
  )
  expect_true(is.null(mes))

  # No message should be delivered if using brute force
  mes <- capture_message(
    find_interactions(d_2vars, outcome = diabetes, brute_force = FALSE)
  )
  expect_false(stringr::str_detect(paste(mes), "This will take"))
})

test_that("test brute force", {
  interactions <- find_interactions(d, outcome = diabetes)
  n_possible <- choose(length(d) - 1, 2) + choose(length(d) - 1, 3)
  expect_equal(length(interactions$significance), n_possible)

  interactions <- find_interactions(d, outcome = diabetes, order = 2)
  n_possible <- choose(length(d) - 1, 2)
  expect_equal(length(interactions$significance), n_possible)
  expect_equal(length(interactions$significance), length(interactions$combinations))
})

test_that("test greedy", {
  # Greedy should get less interactions when significance is .05
  interactions <- find_interactions(d, outcome = diabetes, brute_force = FALSE)
  n_possible <- choose(length(d) - 1, 2) + choose(length(d) - 1, 3)
  expect_true(length(interactions$significance) < n_possible)

  # When the cutoff is negative we should get all the interactions
  interactions <- find_interactions(d, outcome = diabetes, brute_force = FALSE,
                                   greedy_cutoff = -1)
  n_possible <- choose(length(d) - 1, 2) + choose(length(d) - 1, 3)
  expect_true(length(interactions$significance) == n_possible)

  # Test order change to 2nd order, when the cutoff is negative we should get
  # all the interactions
  interactions <- find_interactions(d, outcome = diabetes, brute_force = FALSE,
                                   greedy_cutoff = -1, order = 4)
  n_possible <- choose(length(d) - 1, 2) + choose(length(d) - 1, 3) +
    choose(length(d) - 1, 4)
  expect_true(length(interactions$significance) == n_possible)
  expect_equal(length(interactions$significance), length(interactions$combinations))
})

test_that("test estimate_time", {
  estimate <- estimate_time(5, 1, 2)
  actual <- factorial(5) / (2 * 3 * 2)
  expect_equal(estimate, actual)

  estimate <- estimate_time(5, 1, 3)
  actual <- factorial(5) / (2 * 3 * 2) + factorial(5) / (2 * 3 * 2)
  expect_equal(estimate, actual)
})
