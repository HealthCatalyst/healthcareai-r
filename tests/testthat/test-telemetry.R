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
p <- predict(object = m, newdata = pima_diabetes[1:50, 6:10],
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
  remove_logfiles()
})

test_that("Errors are put in log file properly", {
  # These predict calls have a missing column and should error.
  expect_error(
    predict(object = m, newdata = pima_diabetes[1:50, 7:10],
            write_log = FALSE, prepdata = TRUE)
  )
  expect_error(
    predict(object = m_reloaded, newdata = pima_diabetes[1:50, 7:10],
            write_log = FALSE, prepdata = TRUE)
  )

  # Error should print error message
  expect_warning(predict(object = m,
                         newdata = pima_diabetes[1:50, 7:10],
                         write_log = TRUE, prepdata = TRUE),
                 "insulin")

  # Log should contain error info.
  e <- readLines("telemetry_test_prediction_log.txt")
  expect_true(any(grepl("insulin", e)))

})

test_that("Failure returns warning, blank df, and error info", {
  expect_warning(pe <- predict(object = m_reloaded,
                               newdata = pima_diabetes[1:50, 7:10],
                               write_log = TRUE, prepdata = TRUE),
                 "insulin")
  d_log <- attr(pe, "prediction_log")

  # Tibble should be returned on error
  expect_equal(dim(pe), c(0, 9))
  expect_true(attr(pe, "failed"))
  expect_equal(d_log$outcome_variable, "diabetes")
  expect_false(d_log$predictions_made)
  expect_equal(d_log$n_predictions, NA)
})

test_that("Set and update telemetry functions work", {
  d <- set_inital_telemetry(m)
  expect_equal(dim(d), c(1, 21))
  expect_equal(d$outcome_variable, "diabetes")
  expect_false(d$predictions_made)
  expect_equal(d$n_predictions, NA)

  d_up <- update_telemetry(d, p)
  expect_equal(dim(d_up), c(1, 21))
  expect_equal(d_up$error_message, NA)
  expect_true(is.numeric(d_up$prediction_mean))
  expect_true(is.numeric(d_up$missingness_mean))
})

# Cleanup =======================================
file.remove("telemetry_test_prediction_log.txt")
file.remove("telemetry_test.RDS")
