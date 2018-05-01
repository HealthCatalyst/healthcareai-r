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
# test_that("log_predictions writes info to file correctly", {
#   d <- readLines("telemetry_test_prediction_log.txt")
#   expect_true(any(grepl("name: telemetry_test", d)))
#   expect_true(any(grepl("predicted: diabetes", d)))
#   expect_true(any(grepl("missingness in new data", d)))
# })
#
# test_that("log_predictions returns data correctly", {
#   d_pred <- attr(p_reloaded, "prediction_log")
#   expect_equal(dim(d_pred), c(1, 21))
#   expect_equal(d_pred$loaded_from, "telemetry_test.RDS")
#   expect_equal(d_pred$model_name, "telemetry_test")
#   expect_equal(d_pred$n_predictions, 50)
#   expect_equal(d_pred$outcome_variable, "diabetes")
# })
#
# test_that("log_predictions works without loading from file", {
#   d_pred <- attr(p, "prediction_log")
#   expect_equal(dim(d_pred), c(1, 21))
#   expect_equal(d_pred$loaded_from, "trained_in_memory")
#   expect_equal(d_pred$model_name, "telemetry_test")
#   expect_equal(d_pred$n_predictions, 50)
#   expect_equal(d_pred$outcome_variable, "diabetes")
#   remove_logfiles()
# })

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
  expect_output(predict(object = m,
                        newdata = pima_diabetes[1:50, 7:10],
                        write_log = TRUE, prepdata = TRUE),
                "Error in predict")

  expect_output(predict(object = m_reloaded,
                        newdata = pima_diabetes[1:50, 7:10],
                        write_log = TRUE, prepdata = TRUE),
                "Error in predict")
  # remove_logfiles()
})

test_that("Errors are put in log file properly", {
  # Log should contain error info.
  predict(object = m_reloaded,
          newdata = pima_diabetes[1:50, 7:10],
          write_log = TRUE, prepdata = TRUE)
  e <- read_lines("telemetry_test_prediction_log.txt")
  expect_true(any(grepl("insulin", e)))

  # TODO: tibble should be returned on error
})

test_that("set and update telemetry functions work", {
  d <- set_default_telemetry()
  expect_equal(dim(d), c(1,21))

  d_up <- update_telemetry(d,
                           "file",
                           "target",
                           30,
                           Sys.time(),
                           "model_name",
                           get_pred_summary(
                             data.frame(predicted_col = seq(0, 1, 0.1))),
                           missingness(data.frame(a = c(0,1,2),
                                                  b = c(4, NA, 6)))
  )
  expect_equal(dim(d_up), c(1, 21))
  expect_equal(d_up$error, NA)
  expect_equal(d_up$prediction_mean, 0.5)
  expect_equal(d_up$missingness_mean, 16.65)
})
browser()
# Cleanup =======================================
file.remove("telemetry_test_prediction_log.txt")
file.remove("telemetry_test.RDS")
