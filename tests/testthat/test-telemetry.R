context("Testing telemetry functions")

## Cleanup log files
remove_logfiles <- function() {
  txt_files <- list.files(pattern = "txt$", full.names = TRUE, recursive = FALSE)
  file.remove(txt_files)
}
remove_logfiles()

# Setup ========================================
if (file.exists("telemetry_test_prediction_log.txt"))
  file.remove("telemetry_test_prediction_log.txt")
if (file.exists("telemetry_test.RDS"))
  file.remove("telemetry_test.RDS")

m <- machine_learn(pima_diabetes[1:50, 6:10], outcome = diabetes, models = "rf",
                   model_name = "telemetry_test")
p <- predict(object = m, newdata = pima_diabetes[1:50, 5:10],
                      write_log = TRUE, prepdata = TRUE)

save_models(m, "telemetry_test.RDS")
m_reloaded <- load_models("telemetry_test.RDS")
p_reloaded <- predict(object = m_reloaded, newdata = pima_diabetes[1:50, 6:10],
             write_log = TRUE, prepdata = TRUE)

# Tests =========================================
test_that("log_predictions writes info to file correctly", {
  d <- readLines("telemetry_test_prediction_log.txt")
  expect_true(any(grepl("name: telemetry_test", d)))
  expect_true(any(grepl("predicted: diabetes", d)))
  expect_true(any(grepl("missingness in new data", d)))
})

test_that("log_predictions returns data correctly", {
  d_pred <- attr(p_reloaded, "prediction_log")
  expect_equal(dim(d_pred), c(1, 21))
  expect_equal(d_pred$loaded_from, "telemetry_test.RDS")
  expect_equal(d_pred$model_name, "telemetry_test")
  expect_equal(d_pred$n_predictions, 50)
  expect_equal(d_pred$outcome_variable, "diabetes")
})

test_that("log_predictions works without loading from file", {
  d_pred <- attr(p, "prediction_log")
  expect_equal(dim(d_pred), c(1, 21))
  expect_equal(d_pred$loaded_from, "trained_in_memory")
  expect_equal(d_pred$model_name, "telemetry_test")
  expect_equal(d_pred$n_predictions, 50)
  expect_equal(d_pred$outcome_variable, "diabetes")
})

test_that("errors are put in log file properly", {
expect_error(
  predict(object = m, newdata = pima_diabetes[1:50, 7:10],
          write_log = FALSE, prepdata = TRUE)
  )

# log data should be returned with error
p <- predict(object = m, newdata = pima_diabetes[1:50, 7:10],
        write_log = TRUE, prepdata = TRUE)

# log should contain error info
})

test_that("set and update telemetry functions work", {

})

# Cleanup =======================================
file.remove("telemetry_test_prediction_log.txt")
file.remove("telemetry_test.RDS")
