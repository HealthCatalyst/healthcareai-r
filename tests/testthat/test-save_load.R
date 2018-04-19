context("Checking model saving and loading")

if (file.exists("models.RDS"))
  file.remove("models.RDS")
m <- machine_learn(pima_diabetes[1:20, 8:10], outcome = diabetes, models = "knn")

test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, "models.RDS"))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists("models.RDS"))
})

test_that("load_models works", {
  reloaded_m <- load_models("models.RDS")
  expect_equal(m, reloaded_m)
})

file.remove("models.RDS")
