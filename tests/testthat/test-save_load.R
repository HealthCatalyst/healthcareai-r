context("Checking model saving and loading")

file <- paste0(tempdir(), "/models.RDS")
if (file.exists(file))
  file.remove(file)

m <- machine_learn(pima_diabetes[1:20, 8:10], outcome = diabetes, models = "rf", tune = FALSE)

test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, file,
                                                sanitize_phi = FALSE))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists(file))
  file.remove(file)
})

test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, file, sanitize_phi = FALSE))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists(file))
})

test_that("load_models works", {
  reloaded_m <- load_models(file)
  expect_equal(class(m)[1], "classification_list")
  expect_equal(class(reloaded_m)[1], "classification_list")
  expect_equal(attr(reloaded_m, "loaded_from_rds"), file)
})

test_that("save_models removes data and alerts user", {
  save_messages <- capture_messages(save_models(m, file))
  expect_true(purrr::is_empty(save_messages))

  reloaded_m <- load_models(file)
  expect_true(is.null(attr(reloaded_m, "recipe")$template))
  expect_true(is.null(attr(reloaded_m, "recipe")$orig_data))
})

test_that("save_models keeps data and issues warning if data is present", {
  save_models(m, file, sanitize_phi = FALSE)
  mes <- capture_message(load_models(file))
  expect_true(stringr::str_detect(as.character(mes),
                                   stringr::fixed("PHI protocols apply")))
  reloaded_m <- load_models(file)
  expect_false(is.null(attr(reloaded_m, "recipe")$template))
  expect_false(is.null(attr(reloaded_m, "recipe")$orig_data))
})

test_that("load_model allows user to pick file and alerts message", {
  save_models(m, file)
  with_mock(
    file.choose = function() file,
    mes <- capture_messages(reloaded_m <- load_models())
  )
  expect_equal(class(m)[1], "classification_list")
  expect_equal(class(reloaded_m)[1], "classification_list")
  expect_equal(attr(reloaded_m, "loaded_from_rds"), file)

  expect_equal(
    mes,
    paste0('Loading models. You could automate this with `load_models(\"', file,'\")`\n')
  )
})

file.remove(file)
