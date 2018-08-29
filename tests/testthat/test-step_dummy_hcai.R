context("testing step_dummy_hcai")

data(okc)
okc <- okc[complete.cases(okc),]

rec <- recipe(~ diet + age + height, data = okc)


test_that("testing prep.step_dummy_hcai", {
  diet_orig_levels <- unique(okc$diet)
  diet_orig_levels <- diet_orig_levels[order(diet_orig_levels)]

  dummies <- rec %>% healthcareai:::step_dummy_hcai(diet)
  dummies <- prep(dummies, training = okc)

  diet_values_empty <- attr(dummies$steps[[1]]$levels$diet, "values")
  diet_values <- diet_values_empty[c(4, 1:3, 5:length(diet_values_empty))]

  # ## reorder levels
  dummies <- rec %>% healthcareai:::step_dummy_hcai(diet,  levels = list(diet = diet_values))
  dummies <- prep(dummies, training = okc)
  actual <- attr(dummies$steps[[1]]$levels$diet, "values")
  expect_equal(actual, diet_values)

  dummy_data <- bake(dummies, newdata = okc)
  expect_false("diet_mostly_anything" %in% names(dummy_data))
  expect_true("diet_anything" %in% names(dummy_data))

  ## No levels provided - choose the Mode
  dummies <- rec %>% healthcareai:::step_dummy_hcai(diet,  levels = NULL)
  dummies <- prep(dummies, training = okc)
  actual <- attr(dummies$steps[[1]]$levels$diet, "values")
  expect_equal(actual, diet_values_empty)

  dummy_data <- bake(dummies, newdata = okc)
  expect_false("diet_mostly_anything" %in% names(dummy_data))

  ## Add extra level
  dummies <- rec %>% healthcareai:::step_dummy_hcai(diet,  levels = list(diet = c(diet_values, "extra")))
  dummies <- prep(dummies, training = okc)
  actual <- attr(dummies$steps[[1]]$levels$diet, "values")
  expect_equal(actual, c(diet_values, "extra"))

  dummy_data <- bake(dummies, newdata = okc)
  expect_true("diet_extra" %in% names(dummy_data))

  ## Make dummies from reference level
  dummies <- rec %>% healthcareai:::step_dummy_hcai(diet,  levels = list(diet = c("halal")))
  dummies <- prep(dummies, training = okc)
  actual <- attr(dummies$steps[[1]]$levels$diet, "values")
  expect_equal(actual, diet_orig_levels[c(2, 1, 3:length(diet_orig_levels))])

  dummy_data <- bake(dummies, newdata = okc)
  expect_false("diet_halal" %in% names(dummy_data))
})

test_that("testing dummies", {

})
