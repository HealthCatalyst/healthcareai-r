context("limone")

n <- 5
animals <- tibble::tibble(animal = sample(c("cat", "dog", "mouse"), n, TRUE),
                          y = rnorm(n))

test_that("drop_repeated", {
  list(
    a = tibble::tibble(x = c(1, 1, 2), y = c(3, 3, 4)),
    b = data.frame(varname = rep(letters[1:2], each = 3),
                   x = rep(1:2, 3))
  ) %>%
    drop_repeated("x") %>%
    purrr::map_lgl(~ nrow(.x) == 2) %>%
    all() %>%
    expect_true()
})

test_that("build_one_level_df", {
  animals_replaced <- build_one_level_df(animals, modifiable_variable = "animal", level = "deer")
  expect_true(isTRUE(all.equal(animals$animal, animals_replaced$current_value)))
  expect_true(all(animals_replaced$alt_value == "deer"))
  expect_true(isTRUE(all.equal(animals$y, animals_replaced$y)))

  y_replaced <- build_one_level_df(animals, "y", 2)
  expect_true(isTRUE(all.equal(animals$y, as.numeric(y_replaced$current_value))))
  expect_true(all(y_replaced$alt_value == 2))
  expect_true(isTRUE(all.equal(y_replaced$animal, y_replaced$animal)))
})

test_that("permute_process_variables", {
  perm <- permute_process_variables(animals,
                                    list(animal = c("rat", "gopher"), y = 1:3))
  expect_equal(sum(perm$animal == "rat"), nrow(animals))
  expect_equal(sum(perm$y == 1), sum(perm$y == 2))
  expect_equal(sum(perm$y == 1), nrow(animals))
  expect_setequal(perm$process_variable_name, c("animal", "y"))
})
