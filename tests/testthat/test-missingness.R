context("Checking missingness")

dat <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                  b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                  c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                  d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
dat <- dat %>%
  mutate(across(.cols = 1:3, .fns = as.factor))
dat_strnotfactors <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                                b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                                c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                                d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
dat_strnotfactors <- dat_strnotfactors %>%
  mutate(across(.cols = 1:3, .fns = as.character))
dat_tibble <- tibble(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                     b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
suppressWarnings(out <- missingness(dat))
suppressWarnings(out_vec <- missingness(dat, return_df = FALSE))
manual <- sort(100 * sapply(dat, function(x) sum(is.na(x))) / nrow(dat))

test_that("For a given dataframe, function returns expected output", {
  expect_equal(
    out$percent_missing,
    manual
  )
  expect_equal(
    out$variable,
    names(manual)
  )
})

test_that("For a given matrix, function returns expected output", {
  n <- matrix(c(1, 3, NA, NaN, "NULL", "NAs", "nil", "NONE"),
              nrow = 4, ncol = 2)
  expect_warning(out <- missingness(n))
  expect_equal(unname(out$percent_missing), c(0, 25))
})

test_that("With user defined NA values, function returns expected output", {
  dat2 <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                     b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                     c = c(10, 5, 8, "void", NA, "NULL", NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, "what"))
  expect_warning(out <- missingness(dat2, to_search = "void"), "void")
  expect_equal(unname(out$percent_missing), c(0, 0, 12.5, 12.5))
})

test_that("For a given vector, function returns expected output", {
  vect <- c(1, 2, 3, NA)
  expect_equal(unname(missingness(vect)$percent_missing), 25)
})

test_that("Output is of right class", {
  expect_s3_class(out, "data.frame")
  expect_s3_class(out, "missingness")
  expect_true(is.numeric(out_vec))
  expect_s3_class(out_vec, "missingness")
})

test_that("Variable names aren't repeated in rownames", {
  out <- missingness(data.frame(x = 1:2, y = c("a", NA)))
  expect_true(length(setdiff(rownames(out), out$variable)) > 0)
})

test_that("missingness class is attached to DF", {
  expect_s3_class(out, "missingness")
})

test_that("plot.missingness is registered generic", {
  expect_true("plot.missingness" %in% methods("plot"))
})

test_that("plot.missingness returns a ggplot", {
  expect_s3_class(plot(out, print = FALSE), "gg")
})

test_that("plot.missingness seems to respect options", {
  def <- plot(out, print = FALSE)
  custom <- plot(out, remove_zeros = TRUE)
  expect_false(isTRUE(all.equal(def, custom)))
})

test_that("plot.missingness works on vector input", {
  expect_s3_class(plot(out_vec, print = FALSE), "gg")
})

test_that("missingness preserves small percents-missing", {
  dd <- data.frame(x = c(NA, 1:9999),
                   y = c(rep(NA, 5), rep(0L, 9995)))
  expect_setequal(missingness(dd)$percent_missing,
                  unname(purrr::map_dbl(dd, ~sum(is.na(.x))) * 100 / nrow(dd)))
})

test_that("all rows of a missingness data frame are printed", {
  dd <- data.frame(id = paste0("id", 1:100),
                   tocol = paste0("v", 1:100),
                   val = 1:100) %>%
    pivot(id, tocol, val)
  print_out <- capture_output(print(missingness(dd)))
  expect_true(all(stringr::str_detect(print_out, paste0("v", 1:100))))
})

test_that("printing works whether return_df or not", {
  expect_error(capture_output(print(out)), NA)
  expect_error(capture_output(print(out_vec)), NA)
})

test_that("plot.missingness respects max_char", {
  md <- missingness(data.frame(long_name = NA, longer_still = 1))
  expect_false(isTRUE(all.equal(plot(md, max_char = 7), plot(md))))
})

test_that("test summary.missingness normal", {
  suppressWarnings(
    actual_output <- capture_output(
      actual_result <- missingness(pima_diabetes) %>% summary()
    )
  )

  expect_true(nchar(actual_output) > 0)
  expect_true(grepl("Missingness summary", actual_output))
  expect_true(is.list(actual_result))
  expect_true(rlang::is_named(actual_result))
})

test_that("test summary.missingness no missingness", {
  d <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c("a", "b", "a", "a", "c")
  )
  suppressWarnings(
    actual_output <- capture_output(
      actual_result <- missingness(d) %>% summary()
    )
  )

  expect_equal(actual_result, NULL)
  expect_equal(
    actual_output,
    "Your data does not have any variables with missing values."
  )
})

test_that("make_na - tibble - normal", {
  expected <- tibble(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                     b = c(NA, 0, "na", "None", "none", 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  actual <- make_na(dat_tibble, "blank")
  expect_equal(actual, expected)
})

test_that("make_na - df stringsAsFactors - normal", {
  expected <- tibble(a = factor(c(1, 2, "NA", NA, "none", "??", "?", 5)),
                     b = factor(c(NA, 0, "na", "None", "none", 3, 10, 4)),
                     c = factor(c(10, 5, 8, 1, NA, "NULL", NaN, "Nas")),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  actual <- make_na(dat, "blank")
  expect_equal(actual, expected)
})

test_that("make_na - df strings NOT Factors - normal", {
  expected <- tibble(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                     b = c(NA, 0, "na", "None", "none", 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  actual <- make_na(dat_strnotfactors, "blank")
  expect_equal(actual, expected)
})

test_that("make_na - tibble - character col to numeric", {
  dat_tibble <-
    dat_tibble %>%
    mutate(e = c(1, 2, 3, 4, 5, 6, 7, "none"))
  expected <- tibble(a = c(1, 2, "NA", NA, NA, "??", "?", 5),
                     b = c("blank", 0, "na", "None", NA, 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492),
                     e = c(1, 2, 3, 4, 5, 6, 7, NA))
  actual <- make_na(dat_tibble, "none")
  expect_equal(actual, expected)
})

test_that("make_na - df stringsAsFactors - character col to numeric", {
  dat <-
    dat %>%
    mutate(e = c(1, 2, 3, 4, 5, 6, 7, "none"))
  dat <- as.data.frame(dat)
  expected <- tibble(a = factor(c(1, 2, "NA", NA, NA, "??", "?", 5)),
                     b = factor(c("blank", 0, "na", "None", NA, 3, 10, 4)),
                     c = factor(c(10, 5, 8, 1, NA, "NULL", NaN, "Nas")),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492),
                     e = c(1, 2, 3, 4, 5, 6, 7, NA))
  actual <- make_na(dat, "none")
  expect_equal(actual, expected)
})

test_that("make_na - df strings NOT Factors - character col to numeric", {
  dat_strnotfactors <-
    dat_strnotfactors %>%
    mutate(e = c(1, 2, 3, 4, 5, 6, 7, "none"))
  dat_strnotfactors <- as.data.frame(dat_strnotfactors)
  expected <- tibble(a = c(1, 2, "NA", NA, NA, "??", "?", 5),
                     b = c("blank", 0, "na", "None", NA, 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492),
                     e = c(1, 2, 3, 4, 5, 6, 7, NA))
  actual <- make_na(dat_strnotfactors, "none")
  expect_equal(actual, expected)
})

test_that("make_na - tibble - replace NULL", {
  expected <- tibble(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                     b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, NA, NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  actual <- make_na(dat_tibble, "NULL")
  expect_equal(actual, expected)
})

test_that("make_na - df stringsAsFactors - replace NULL", {
  expected <- tibble(a = as.factor(c(1, 2, "NA", NA, "none", "??", "?", 5)),
                     b = as.factor(c("blank", 0, "na", "None", "none", 3, 10, 4)),
                     c = as.factor(c(10, 5, 8, 1, NA, NA, NaN, "Nas")),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  actual <- make_na(dat, "NULL")
  expect_equal(actual, expected)
})

test_that("make_na - df strings NOT Factors - replace NULL", {
  expected <- tibble(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                     b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, NA, NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  actual <- make_na(dat_strnotfactors, "NULL")
  expect_equal(actual, expected)
})

test_that("make_na - tibble - replace many", {
  expected <- tibble(a = c(1, 2, NA, NA, NA, NA, NA, 5),
                     b = c(NA, 0, NA, NA, NA, 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, NA, NA, NA),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  missing_values <-
    c("NULL", "none", NaN, "Nas", "na", "??", "?", "NA", "blank", "None")
  actual <- make_na(dat_tibble, missing_values)
  expect_equal(actual, expected)
})

test_that("make_na - df stringsAsFactors - replace many", {
  expected <- tibble(a = c(1, 2, NA, NA, NA, NA, NA, 5),
                     b = c(NA, 0, NA, NA, NA, 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, NA, NA, NA),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  missing_values <-
    c("NULL", "none", NaN, "Nas", "na", "??", "?", "NA", "blank", "None")
  actual <- make_na(dat, missing_values)
  expect_equal(actual, expected)
})

test_that("make_na - df strings NOT Factors - replace many", {
  expected <- tibble(a = c(1, 2, NA, NA, NA, NA, NA, 5),
                     b = c(NA, 0, NA, NA, NA, 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, NA, NA, NA),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  missing_values <-
    c("NULL", "none", NaN, "Nas", "na", "??", "?", "NA", "blank", "None")
  actual <- make_na(dat_strnotfactors, missing_values)
  expect_equal(actual, expected)
})


test_that("make_na - df strings NOT Factors - replace many", {
  expected <- tibble(a = c(1, 2, NA, NA, NA, NA, NA, 5),
                     b = c(NA, 0, NA, NA, NA, 3, 10, 4),
                     c = c(10, 5, 8, 1, NA, NA, NA, NA),
                     d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  missing_values <-
    c("NULL", "none", NaN, "Nas", "na", "??", "?", "NA", "blank", "None")
  actual <- make_na(dat_strnotfactors, missing_values)
  expect_equal(actual, expected)
})

test_that("make_na - test df or tibble", {
  expect_error(make_na(c(1, 2), "NA"))
})

test_that("make_na - test atomic", {
  expect_error(make_na(dat, list()))
})
