context("testing step_dummy_hcai")

rec <- recipes::recipe(head(pima_diabetes), ~.)

test_that("testing prep.step_dummy_hcai", {
  weight_class_orig_levels <- unique(pima_diabetes$weight_class)
  weight_class_orig_levels <- weight_class_orig_levels[!is.na(weight_class_orig_levels)]
  weight_class_orig_levels <- weight_class_orig_levels[order(weight_class_orig_levels)]

  dummies <- rec %>% healthcareai:::step_dummy_hcai(weight_class)
  expect_warning(
    dummies <- recipes::prep(dummies, training = pima_diabetes)
  )

  weight_class_values_empty <- attr(dummies$steps[[1]]$levels$weight_class, "values")
  weight_class_values <- weight_class_values_empty[c(4, 1:3, 5:length(weight_class_values_empty))]

  # ## reorder levels
  dummies <- rec %>% healthcareai:::step_dummy_hcai(weight_class,  levels = list(weight_class = weight_class_values))
  expect_warning(
    dummies <- prep(dummies, training = pima_diabetes)
  )
  actual <- attr(dummies$steps[[1]]$levels$weight_class, "values")
  expect_equal(actual, weight_class_values)

  expect_warning(
    dummy_data <- bake(dummies, new_data = pima_diabetes)
  )
  expect_false("weight_class_overweight" %in% names(dummy_data))
  expect_true("weight_class_obese" %in% names(dummy_data))

  # No levels provided - choose the Mode
  dummies <- rec %>% healthcareai:::step_dummy_hcai(weight_class,  levels = NULL)
  expect_warning(
    dummies <- prep(dummies, training = pima_diabetes)
  )
  actual <- attr(dummies$steps[[1]]$levels$weight_class, "values")
  expect_equal(actual, weight_class_values_empty)

  expect_warning(
    dummy_data <- bake(dummies, new_data = pima_diabetes)
  )
  expect_false("weight_class_obese" %in% names(dummy_data))

  # Add extra level
  dummies <- rec %>% healthcareai:::step_dummy_hcai(weight_class,  levels = list(weight_class = c(weight_class_values, "missing")))
  expect_warning(
    dummies <- prep(dummies, training = pima_diabetes)
  )
  actual <- attr(dummies$steps[[1]]$levels$weight_class, "values")
  expect_equal(actual, c(weight_class_values, "missing"))

  expect_warning(
    dummy_data <- bake(dummies, new_data = pima_diabetes)
  )
  expect_true("weight_class_missing" %in% names(dummy_data))

  # Make dummies from reference level
  dummies <- rec %>% healthcareai:::step_dummy_hcai(weight_class,  levels = list(weight_class = "normal"))
  expect_warning(
    dummies <- prep(dummies, training = pima_diabetes)
  )
  actual <- attr(dummies$steps[[1]]$levels$weight_class, "values")
  expect_equal(actual, weight_class_orig_levels[c(2, 1, 3:length(weight_class_orig_levels))])

  expect_warning(
    dummy_data <- bake(dummies, new_data = pima_diabetes)
  )
  expect_false("weight_class_normal" %in% names(dummy_data))
})


test_that("print step_dummy_hcai", {
  dummy <- rec %>%
    step_dummy_hcai(all_nominal())
  expect_error(out <- capture_output(print(dummy)), NA)
  expect_true(grepl("Dummy variables from all_nominal()", out))

  expect_warning(
    dummy <-
      dummy %>%
      prep(training = pima_diabetes)
  )
  expect_error(out <- capture_output(print(dummy)), NA)
  expect_true(
    grepl("Dummy variables from weight_class and diabetes \\[trained\\]", out)
  )
})

test_that("test numerical input throws error", {
  dummy <-
    rec %>%
    step_dummy_hcai(pregnancies)

  expect_error(
    prep(dummy, training = pima_diabetes),
    "not factor vectors:"
  )
})

test_that("converts data to tibble", {
  dummy <-
    rec %>%
    step_dummy_hcai(all_nominal())

  expect_warning(
    d <-
      prep(dummy, training = data.frame(pima_diabetes)) %>%
      bake(new_data = data.frame(pima_diabetes))
  )
  expect_true(is_tibble(d))
})

test_that("test step ref_levels and dummy attributes", {
  dummy_rec <-
    rec %>%
    step_dummy_hcai(all_nominal())

  expect_warning(
    dummy_rec <-
      prep(dummy_rec, training = data.frame(pima_diabetes))
  )

  dummies <- dummy_rec$steps[1][[1]]$dummies
  ref_levels <- dummy_rec$steps[1][[1]]$ref_levels

  expect_true("diabetes_Y" %in% dummies$dummy)
  expect_true("weight_class_morbidly.obese" %in% dummies$dummy)
  expect_true("obese" %in% dummies$ref)
  expect_true("N" %in% dummies$ref)

  expect_equal(ref_levels, c(weight_class = "obese", diabetes = "N"))
})

test_that("test tidy prints correctly", {
  weight_class_orig_levels <- unique(pima_diabetes$weight_class)
  weight_class_orig_levels <- weight_class_orig_levels[!is.na(weight_class_orig_levels)]
  weight_class_orig_levels <- weight_class_orig_levels[order(weight_class_orig_levels)]

  dummies_rec <- rec %>% healthcareai:::step_dummy_hcai(weight_class,
                                                        id = "bagimpute_rN6wq")

  exp <- tibble(
    terms = c("weight_class"),
    id = c("bagimpute_rN6wq")
  )
  expect_equal(
    exp,
    tidy(dummies_rec$steps[[1]])
  )

  expect_warning(
    dummies_rec <- recipes::prep(dummies_rec, training = pima_diabetes)
  )
  expect_equal(
    exp,
    tidy(dummies_rec$steps[[1]])
  )
})
