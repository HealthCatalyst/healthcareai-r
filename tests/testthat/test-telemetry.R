context("Testing telemetry functions")

if (file.exists("telemetry_test_prediction_log.txt"))
  file.remove("telemetry_test_prediction_log.txt")
m <- machine_learn(pima_diabetes[1:20, 8:10], outcome = diabetes, models = "rf",
                   model_name = "telemetry_test")
predict(object = m, newdata = pima_diabetes[1:10,], write_log = TRUE)
d <- readLines("telemetry_test_prediction_log.txt")

test_that("log_predictions writes info to file correctly", {
  expect_true(any(grepl("name: telemetry_test", d)))
  expect_true(any(grepl("predicted: diabetes", d)))
  expect_true(any(grepl("missingness in new data", d)))
})

file.remove("telemetry_test_prediction_log.txt")
