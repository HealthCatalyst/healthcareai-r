context("Checking model saving and loading")

if (file.exists("models.RDS"))
  file.remove("models.RDS")
m <- machine_learn(pima_diabetes[1:20, 8:10], outcome = diabetes, models = "rf", tune = FALSE)

test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, "models.RDS",
                                                sanitize_phi = FALSE))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists("models.RDS"))
})

test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, sanitize_phi = FALSE))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists("models.RDS"))
})

test_that("load_models works", {
  reloaded_m <- load_models("models.RDS")
  expect_equal(class(m)[1], "classification_list")
  expect_equal(class(reloaded_m)[1], "classification_list")
  expect_equal(attr(reloaded_m, "loaded_from_rds"), "models.RDS")
})

test_that("save_models removes data", {
  save_messages <- capture_messages(save_models(m))
  expect_true(stringr::str_detect(save_messages,
                                  stringr::fixed("sanitize_phi = FALSE")))

  reloaded_m <- load_models("models.RDS")
  expect_true(is.na(unique(attr(reloaded_m, "recipe")$template$age)))
})

test_that("save_models keeps data", {
  save_models(m, sanitize_phi = FALSE)
  reloaded_m <- load_models("models.RDS")
  expect_false(all(is.na(unique(attr(reloaded_m, "recipe")$template$age))))
})

test_that("save_models has removed_data attribute set to true when TRUE", {
  save_models(m)
  reloaded_m <- load_models("models.RDS")
  expect_true(attr(reloaded_m, "sanitize_phi") == TRUE)
})

test_that("save_models has removed_data attribute set is null when FALSE", {
  save_models(m, sanitize_phi = FALSE)
  reloaded_m <- load_models("models.RDS")
  expect_true(is.null(attr(reloaded_m, "sanitize_phi")))
})

file.remove("models.RDS")
