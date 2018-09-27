context("Checking model saving and loading")

if (file.exists("models.RDS"))
  file.remove("models.RDS")
if (file.exists("modelsPHI.RDS"))
  file.remove("modelsPHI.RDS")
if (file.exists("newfile.RDS"))
  file.remove("newfile.RDS")
if (file.exists("newfilePHI.RDS"))
  file.remove("newfilePHI.RDS")

m <- machine_learn(pima_diabetes[1:20, 8:10], outcome = diabetes, models = "rf", tune = FALSE)

test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, "models.RDS",
                                                sanitize_phi = FALSE))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists("modelsPHI.RDS"))
})

test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, sanitize_phi = FALSE))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists("modelsPHI.RDS"))
})

test_that("load_models works", {
  reloaded_m <- load_models("modelsPHI.RDS")
  expect_equal(class(m)[1], "classification_list")
  expect_equal(class(reloaded_m)[1], "classification_list")
  expect_equal(attr(reloaded_m, "loaded_from_rds"), "modelsPHI.RDS")
})

test_that("save_models removes data and alerts user", {
  save_messages <- capture_messages(save_models(m))
  expect_true(stringr::str_detect(save_messages,
                                  stringr::fixed("sanitize_phi = FALSE")))

  reloaded_m <- load_models("models.RDS")
  expect_true(is.null(attr(reloaded_m, "recipe")$template))
})

test_that("save_models keeps data and issues warning if data is present", {
  save_models(m, sanitize_phi = FALSE)
  mes <- capture_message(load_models("modelsPHI.RDS"))
  expect_true(stringr::str_detect(as.character(mes),
                                   stringr::fixed("contains PHI!")))
  reloaded_m <- load_models("modelsPHI.RDS")
  expect_false(is.null(attr(reloaded_m, "recipe")$template))
})

test_that("save_models creates correct file", {
  if (file.exists("models.RDS"))
    file.remove("models.RDS")
  if (file.exists("modelsPHI.RDS"))
    file.remove("modelsPHI.RDS")

  save_models(m)
  expect_true(file.exists("models.RDS"))
  expect_true(!file.exists("modelsPHI.RDS"))

  file.remove("models.RDS")

  save_models(m, sanitize_phi = FALSE)
  expect_true(file.exists("modelsPHI.RDS"))
  expect_true(!file.exists("models.RDS"))

  file.remove("modelsPHI.RDS")

  save_models(m, "newfile.RDS", sanitize_phi = FALSE)
  expect_true(file.exists("newfilePHI.RDS"))
  expect_true(!file.exists("newfile.RDS"))
  file.remove("newfilePHI.RDS")

  save_models(m, "newfile.RDS")
  expect_true(file.exists("newfile.RDS"))
  expect_true(!file.exists("newfilePHI.RDS"))
  file.remove("newfile.RDS")
})

if (file.exists("models.RDS"))
  file.remove("models.RDS")
if (file.exists("modelsPHI.RDS"))
  file.remove("modelsPHI.RDS")
if (file.exists("newfile.RDS"))
  file.remove("newfile.RDS")
if (file.exists("newfilePHI.RDS"))
  file.remove("newfilePHI.RDS")
  
test_that("load_model allows user to pick file and alerts message", {
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
