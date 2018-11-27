context("Checking model saving and loading")

if (file.exists("models.RDS"))
  file.remove("models.RDS")

m <- machine_learn(pima_diabetes[1:20, 8:10], outcome = diabetes, models = "rf", tune = FALSE)

test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, "models.RDS",
                                                sanitize_phi = FALSE))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists("models.RDS"))
  file.remove("models.RDS")
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

test_that("save_models removes data and alerts user", {
  save_messages <- capture_messages(save_models(m))
  expect_true(purrr::is_empty(save_messages))

  reloaded_m <- load_models("models.RDS")
  expect_true(is.null(attr(reloaded_m, "recipe")$template))
  expect_true(is.null(attr(reloaded_m, "recipe")$orig_data))
})

test_that("save_models keeps data and issues warning if data is present", {
  save_models(m, sanitize_phi = FALSE)
  mes <- capture_message(load_models("models.RDS"))
  expect_true(stringr::str_detect(as.character(mes),
                                   stringr::fixed("PHI protocols apply")))
  reloaded_m <- load_models("models.RDS")
  expect_false(is.null(attr(reloaded_m, "recipe")$template))
  expect_false(is.null(attr(reloaded_m, "recipe")$orig_data))
})

test_that("load_model allows user to pick file and alerts message", {
  save_models(m)
  with_mock(
    file.choose = function() "models.RDS",
    mes <- capture_messages(reloaded_m <- load_models())
  )
  expect_equal(class(m)[1], "classification_list")
  expect_equal(class(reloaded_m)[1], "classification_list")
  expect_equal(attr(reloaded_m, "loaded_from_rds"), "models.RDS")

  expect_equal(
    mes,
    "Loading models. You could automate this with `load_models(\"models.RDS\")`\n"
  )
})

file.remove("models.RDS")
