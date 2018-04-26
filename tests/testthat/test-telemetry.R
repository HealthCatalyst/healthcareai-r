context("Testing telemetry functions")

if (file.exists("telemetry_test_prediction_log.txt"))
  file.remove("telemetry_test_prediction_log.txt")
m <- machine_learn(pima_diabetes[1:20, 8:10], outcome = diabetes, models = "rf",
                   model_name = "telemetry_test")
predict(object = m, newdata = pima_diabetes[1:10,], write_log = TRUE)

predict()
test_that("save_models works and issues PHI message", {
  save_messages <- capture_messages(save_models(m, "models.RDS"))
  expect_true(stringr::str_detect(save_messages, stringr::fixed("PHI")))
  expect_true(file.exists("models.RDS"))
})

file.remove("telemetry_test_prediction_log.txt")
