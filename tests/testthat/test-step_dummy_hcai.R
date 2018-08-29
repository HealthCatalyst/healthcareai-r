context("testing step_dummy_hcai")


rec <- recipes::recipe(head(pima_diabetes), ~.)

test_that("testing prep.step_dummy_hcai", {
  weight_class_orig_levels <- unique(pima_diabetes$weight_class)
  weight_class_orig_levels <- weight_class_orig_levels[!is.na(weight_class_orig_levels)]
  weight_class_orig_levels <- weight_class_orig_levels[order(weight_class_orig_levels)]

  dummies <- rec %>% healthcareai:::step_dummy_hcai(weight_class)
  expect_warning(
    dummies <- prep(dummies, training = pima_diabetes)
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
    dummy_data <- bake(dummies, newdata = pima_diabetes)
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
    dummy_data <- bake(dummies, newdata = pima_diabetes)
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
    dummy_data <- bake(dummies, newdata = pima_diabetes)
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
    dummy_data <- bake(dummies, newdata = pima_diabetes)
  )
  expect_false("weight_class_normal" %in% names(dummy_data))
})
