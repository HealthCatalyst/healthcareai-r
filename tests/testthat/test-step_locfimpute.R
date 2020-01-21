context("Test step_locfimpute")

# Setup ------------------------------------------------------------------------
d <- data.frame(
  num_1 = c(1, NA, 2, NA),
  num_2 = c(NA, 1, 2, 3),
  char_1 = c("a", NA, "b", NA),
  char_2 = c(NA, "a", "b", "c"),
  fac_1 = factor(c("a", NA, "b", NA)),
  fac_2 = factor(c(NA, "a", "b", "c")),
  stringsAsFactors = FALSE
)
rec <- recipes::recipe(formula = "~.", d) #nolint

# Tests ------------------------------------------------------------------------
test_that("Impute numeric", {
  out <-
    rec %>%
    step_locfimpute(all_numeric()) %>%
    prep() %>%
    bake(new_data = d)

  expect_false(any(is.na(out$num_1)))
  expect_equal(out %>% slice(2) %>% pull(num_1), 1)
  expect_equal(out %>% slice(4) %>% pull(num_1), 2)
  expect_false(any(is.na(out$num_2)))
  expect_equal(out %>% slice(1) %>% pull(num_2), 1)
})

test_that("Imputes character", {
  out <-
    rec %>%
    step_locfimpute(char_1, char_2) %>%
    prep() %>%
    bake(new_data = d)

  expect_false(any(is.na(out$char_1)))
  expect_equal(out %>% slice(2) %>% pull(char_1) %>% as.character(), "a")
  expect_equal(out %>% slice(4) %>% pull(char_1) %>% as.character(), "b")
  expect_false(any(is.na(out$char_2)))
  expect_equal(out %>% slice(1) %>% pull(char_2) %>% as.character(), "a")
})

test_that("Imputes factor", {
  out <-
    rec %>%
    step_locfimpute(fac_1, fac_2) %>%
    prep() %>%
    bake(new_data = d)

  expect_false(any(is.na(out$fac_1)))
  expect_equal(out %>% slice(2) %>% pull(fac_1) %>% as.character(), "a")
  expect_equal(out %>% slice(4) %>% pull(fac_1) %>% as.character(), "b")
  expect_false(any(is.na(out$fac_2)))
  expect_equal(out %>% slice(1) %>% pull(fac_2) %>% as.character(), "a")
})

test_that("Returns Tibble", {
  out <-
    rec %>%
    step_locfimpute(fac_1, fac_2) %>%
    prep() %>%
    bake(new_data = d)
  expect_true(tibble::is_tibble(out))
})

test_that("Imputes Tibble", {
  out <-
    recipes::recipe(formula = "~.", tibble::as_tibble(d)) %>% #nolint
    step_locfimpute(all_nominal(), all_numeric()) %>%
    prep() %>%
    bake(new_data = d)

  expect_false(any(is.na(out$num_1)))
  expect_equal(out %>% slice(2) %>% pull(num_1), 1)
  expect_equal(out %>% slice(4) %>% pull(num_1), 2)
  expect_false(any(is.na(out$num_2)))
  expect_equal(out %>% slice(1) %>% pull(num_2), 1)

  expect_false(any(is.na(out$char_1)))
  expect_equal(out %>% slice(2) %>% pull(char_1) %>% as.character(), "a")
  expect_equal(out %>% slice(4) %>% pull(char_1) %>% as.character(), "b")
  expect_false(any(is.na(out$char_2)))
  expect_equal(out %>% slice(1) %>% pull(char_2) %>% as.character(), "a")

  expect_false(any(is.na(out$fac_1)))
  expect_equal(out %>% slice(2) %>% pull(fac_1) %>% as.character(), "a")
  expect_equal(out %>% slice(4) %>% pull(fac_1) %>% as.character(), "b")
  expect_false(any(is.na(out$fac_2)))
  expect_equal(out %>% slice(1) %>% pull(fac_2) %>% as.character(), "a")
})

test_that("Print works both before and after prepping", {
  tmp_rec <-
    rec %>%
    step_locfimpute(char_1, char_2)

  out <- capture_output(print(tmp_rec))
  expect_true(stringr::str_detect(out, "LOCF Imputation for char_1, char_2"))
  expect_false(stringr::str_detect(out, "trained"))

  prepped_rec <-
    tmp_rec %>%
    prep()

  out <- capture_output(print(prepped_rec))
  expect_true(stringr::str_detect(out, "LOCF Imputation for char_1, char_2"))
  expect_true(stringr::str_detect(out, "trained"))
})

test_that("Tidy works both before and after training", {
  tmp_rec <-
    rec %>%
    step_locfimpute(char_1, char_2)

  out <- tidy(tmp_rec$steps[[1]])
  expect_false(any(out %>% pull(trained)))

  prepped <-
    tmp_rec %>%
    prep()

  out <- tidy(prepped$steps[[1]])
  expect_true(all(out %>% pull(trained)))
})
