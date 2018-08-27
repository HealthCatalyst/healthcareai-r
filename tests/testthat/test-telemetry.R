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
             write_log = TRUE)

save_models(m, "telemetry_test.RDS")
m_reloaded <- load_models("telemetry_test.RDS")
p_reloaded <- predict(object = m_reloaded, newdata = pima_diabetes[1:50, 6:10],
                      write_log = TRUE)

# Tests =========================================
test_that("log_predictions writes info to file correctly", {
  d <- readLines("telemetry_test_prediction_log.txt")
  expect_true(any(grepl("name: telemetry_test", d)))
  expect_true(any(grepl("predicted: diabetes", d)))
  expect_true(any(grepl("missingness in new data", d)))
})

test_that("log_predictions returns data correctly", {
  d_pred <- attr(p_reloaded, "prediction_log")
  expect_equal(dim(d_pred), c(1, 23))
  expect_equal(d_pred$loaded_from, "telemetry_test.RDS")
  expect_equal(d_pred$model_name, "telemetry_test")
  expect_equal(d_pred$n_predictions, 50)
  expect_equal(d_pred$outcome_variable, "diabetes")
  expect_equal(class(d_pred$run_time), "numeric")
})

test_that("log_predictions works without loading from file", {
  d_pred <- attr(p, "prediction_log")
  expect_equal(dim(d_pred), c(1, 23))
  expect_equal(d_pred$loaded_from, "trained_in_memory")
  expect_equal(d_pred$model_name, "telemetry_test")
  expect_equal(d_pred$n_predictions, 50)
  expect_equal(d_pred$outcome_variable, "diabetes")
  expect_equal(class(d_pred$run_time), "numeric")
  remove_logfiles()
})

test_that("Errors are put in log file properly", {
  # These predict calls have a missing column and should error on predict.
  # They also warn on prep
  expect_error(
    expect_warning(
      predict(object = m, newdata = pima_diabetes[1:50, 7:10],
              write_log = FALSE)
    )
  )
  expect_error(
    expect_warning(
      predict(object = m_reloaded, newdata = pima_diabetes[1:50, 7:10],
              write_log = FALSE)
    )
  )

  # Error should print error message
  expect_warning(p <- predict(object = m,
                              newdata = pima_diabetes[1:50, 7:10],
                              write_log = TRUE),
                 "insulin")
  # Log should contain error info.
  e <- readLines("telemetry_test_prediction_log.txt")
  expect_true(any(grepl("insulin", e)))

})

test_that("Warnings are parsed correctly", {
  nd <- pima_diabetes[1:50, 6:10]
  nd$weight_class[1] <- "jabba"
  expect_warning(p <- predict(object = m, newdata = nd,
                              write_log = TRUE),
                 "jabba")
  expect_true(any(grepl("jabba",
                        attr(p, "prediction_log")$warnings)))
})

test_that("Failure returns warning, blank df, and error info", {
  expect_warning(pe <- predict(object = m_reloaded,
                               newdata = pima_diabetes[1:50, 7:10],
                               write_log = TRUE, prepdata = TRUE),
                 "insulin")
  d_log <- attr(pe, "prediction_log")

  # Tibble should be returned on error
  expect_equal(dim(pe), c(0, 6))
  expect_true(attr(pe, "failed"))
  expect_equal(d_log$outcome_variable, "diabetes")
  expect_false(d_log$predictions_made)
  expect_equal(d_log$n_predictions, 0)
  expect_equal(names(pe), names(p))
  expect_equal(class(d_log$run_time), "numeric")
})

test_that("Set and update telemetry functions work", {
  d <- set_inital_telemetry(extract_model_info(m))
  expect_equal(dim(d), c(1, 23))
  expect_equal(d$outcome_variable, "diabetes")
  expect_false(d$predictions_made)
  expect_equal(d$n_predictions, 0)

  d_up <- update_telemetry(d, p)
  expect_equal(dim(d_up), c(1, 23))
  expect_equal(d_up$error_message, NA)
  expect_true(is.numeric(d_up$prediction_mean))
  expect_true(is.numeric(d_up$missingness_mean))
})

test_that("parse safe and quiet works", {
  mi <- extract_model_info(m)
  x <- list(result = p, error = NULL, warnings = NULL)
  expect_equal(class(parse_safe_n_quiet(x, mi, m))[1], "predicted_df")
  expect_equal(dim(parse_safe_n_quiet(x, mi, m)), c(50, 6))

  x <- list(result = p, error = NULL, warnings = "grrrr")
  expect_warning(res <- parse_safe_n_quiet(x, mi, m), "grrr")
  expect_equal(class(res)[1], "predicted_df")
  expect_equal(dim(res), c(50, 6))

  x <- list(result = NULL, error = list(message = "ARHT!", call = "this spot"),
            warnings = NULL)
  expect_warning(res <- parse_safe_n_quiet(x, mi, m), "ARHT")
  expect_equal(class(res)[1], "tbl_df")
  expect_equal(dim(res), c(0,
                           6))
})


# Cleanup =======================================
file.remove("telemetry_test_prediction_log.txt")
file.remove("telemetry_test.RDS")
