context("pip")
library(dplyr)

set.seed(5470)
n <- 10
animals <- tibble::tibble(id_column = letters[1:n],
                          animal = sample(c("cat", "dog", "mouse", "rabbit"), n, TRUE),
                          weight = rexp(n, .5),
                          super =  sample(c("yes", "no"), n, TRUE),
                          num2 = rpois(n, 5),
                          y = rnorm(n))
suppressWarnings({
  animodel <- machine_learn(animals, id_column, outcome = y, tune = FALSE, models = c("xgb", "glm"))
  glm_model <- animodel["glmnet"]
  animodel <- animodel["eXtreme Gradient Boosting"]
})
alt_list <- list(animal = c("dog", "cat"), weight = 0, super = c("yes", "no"))
def_pip <- pip(animodel, animals, new_values = alt_list)

test_that("build_one_level_df", {
  animals_replaced <- build_one_level_df(animals, variable = "animal", level = "deer", variable_direction = NULL)
  expect_true(isTRUE(all.equal(animals$animal, animals_replaced$current_value)))
  expect_true(all(animals_replaced$alt_value == "deer"))
  expect_true(isTRUE(all.equal(animals$y, animals_replaced$y)))

  y_replaced <- build_one_level_df(animals, "y", 2, variable_direction = NULL)
  expect_true(isTRUE(all.equal(animals$y, as.numeric(y_replaced$current_value))))
  expect_true(all(y_replaced$alt_value == 2))
  expect_true(isTRUE(all.equal(y_replaced$animal, y_replaced$animal)))
})

test_that("permute_process_variables", {
  perm <- permute_process_variables(animals,
                                    list(animal = c("rat", "gopher"), y = 1:3),
                                    variable_direction = NULL)
  expect_equal(sum(perm$animal == "rat"), nrow(animals))
  expect_equal(sum(perm$y == 1), sum(perm$y == 2))
  expect_equal(sum(perm$y == 1), nrow(animals))
  expect_setequal(perm$process_variable_name, c("animal", "y"))
})

test_that("id columns are retained", {
  # From model_list
  expect_true("id_column" %in% names(def_pip))
  # Explicitly given
  ani2 <- mutate(animals, id2 = 1:n)
  m2 <- machine_learn(ani2, id_column, outcome = y, tune = FALSE, models = "xgb")
  pip2 <- pip(m2, ani2[1, ], alt_list)
  expect_true("id_column" %in% names(pip2))
  expect_false("id2" %in% names(pip2))
  pip3 <- pip(m2, ani2[1, ], alt_list, id = id2)
  expect_true("id2" %in% names(pip3))
})

test_that("repeated_factors works", {
  max_count <- function(d)
    count(d, id_column) %>% pull(n) %>% max()
  expect_true(max_count(def_pip) <= 3)
  expect_true(max_count(pip(animodel, animals, new_values = alt_list, n = 2)) <= 2)
  expect_true(max_count(pip(animodel, animals, new_values = alt_list, n = 1)) <= 1)
})

test_that("pip warns if unused variables are provided as potential modifieds", {
  # Ensure that animal was regularized to zero:
  expect_false("animal" %in% interpret(glm_model)$variable)
  expect_warning(pip(glm_model, animals, new_values = alt_list), "animal")
})

test_that("smaller_better", {
  rev_pip <- pip(animodel, animals, new_values = alt_list, smaller_better = FALSE)
  inner_join(
    filter(rev_pip, improvement > 0),
    filter(def_pip, improvement > 0),
    by = c("id_column", "original_value", "modified_value")
  ) %>%
    nrow() %>%
    expect_equal(0)
})

test_that("variable_direction", {
  expect_error(pip(animodel, animals, alt_list, variable_direction = c(weight = 0)),
               "-1 or 1")
  expect_error(pip(animodel, animals, alt_list, variable_direction = c(super = 1)),
               "numeric")
  expect_error(pip(animodel, animals, alt_list, variable_direction = c(not_a_var = 1)),
               "not present")

  weight_down <- pip(animodel, animals, alt_list, variable_direction = list(weight = -1))
  weight_down %>%
    filter(variable == "weight") %>%
    mutate(weight_decrease = modified_value <= original_value) %>%
    summarize(all_down = all(weight_decrease)) %>%
    pull(all_down) %>%
    expect_true()

  weight_up <- pip(animodel, animals, alt_list, variable_direction = list(weight = 1))
  weight_up %>%
    filter(variable == "weight") %>%
    mutate(weight_increase = modified_value >= original_value) %>%
    summarize(all_up = all(weight_increase)) %>%
    pull(all_up) %>%
    expect_true()

  new_vals <- list(weight = c(.1, .5, 1), num2 = c(5, 7))
  no_dir <- pip(animodel, animals, new_values = new_vals)
  w_dir <- pip(animodel, animals, new_values = new_vals,
               variable_direction = c(weight = -1, num2 = 1))
  no_dir %>%
    filter(variable == "weight", modified_value <= original_value) %>%
    select(-impact_rank) %>%
    all.equal(select(filter(w_dir, variable == "weight"), -impact_rank)) %>%
    isTRUE() %>%
    expect_true()

  no_dir %>%
    filter(variable == "num2", modified_value >= original_value) %>%
    select(-impact_rank) %>%
    all.equal(select(filter(w_dir, variable == "num2"), -impact_rank)) %>%
    isTRUE() %>%
    expect_true()
})
