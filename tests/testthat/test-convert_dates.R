context("Testing convert_date_cols")

# Setup ------------------------------------------------------------------------
d <- data.frame(
  a_DTS = c("2018-3-25", "2018-3-26"),
  b_DTS = c("2018-03-25", "2018-03-26"),
  c_DTS = c("3-25-2018", "3-26-2018"),
  d_DTS = c("03-25-2018", "03-26-2018"),
  e_DTS = c("Mar 25 2018", "Mar 26 2018"),
  f_DTS = c("March 25th, 2018", "March 26th, 2018"),
  stringsAsFactors = FALSE
)

dt <- data.frame(
  aa_DTS = c("2018-3-25 22:30:00",  "2018-3-26 22:30:00"),
  bb_DTS = c("2018-03-25 22:30:00",  "2018-03-26 22:30:00"),
  cc_DTS = c("3-25-2018 22:30:00",  "3-26-2018 22:30:00"),
  dd_DTS = c("03-25-2018 22:30:00",  "03-26-2018 22:30:00"),
  ee_DTS = c("Mar 25 2018 22:30:00",  "Mar 26 2018 22:30:00"),
  ff_DTS = c("March 25th, 2018 22:30:00", "March 26th, 2018 22:30:00"),
  stringsAsFactors = FALSE
)

d_mixed <- data.frame(
  z_DTS = c("2018-3-5", "2018-3-26", "2019-6-5"),
  b_nums = c(2, 4, 6),
  c_DTS = c("03-05-2018", "03-26-2018", "06-05-2019"),
  d_chars = c("a", "b", "d"),
  e_date = lubridate::mdy(c("3-05-2018", "3-26-2018", "06-05-2019")),
  f_DTS = c("2000-1-1 13:30:05", "2000-1-10 1:50:10", "2001-5-10 12:50:10"),
  stringsAsFactors = FALSE
)

# Tests ------------------------------------------------------------------------
test_that("convert dates finds bad DTS columns", {
  d <- dplyr::bind_cols(d,
    not_DTS = c("string cheese", "string cheese"),
    typo_DTS = c("Marches 25th, 2018 22:30:00",
                 "Marches 25th, 2018 22:30:00"))
  expect_error(convert_date_cols(d), "not_DTS")
})

test_that("convert dates finds bad DTS columns in tibble", {
  d <- dplyr::bind_cols(tibble::as_tibble(d),
                        not_DTS = c("string cheese", "string cheese"),
                        typo_DTS = c("Marches 25th, 2018 22:30:00",
                                     "Marches 26th, 2018 22:30:00"))
  expect_error(convert_date_cols(d), "not_DTS")
})

test_that("all common formats are converted", {
  out <- convert_date_cols(d)
  expect_true(all(purrr::map_lgl(out, is.Date)))
  expect_equal(dim(out), c(2, 6))

  out <- convert_date_cols(dt)
  expect_true(all(purrr::map_lgl(out, is.POSIXt)))
  expect_equal(dim(out), c(2, 6))
})

test_that("all common formats are converted in tibble", {
  out <- convert_date_cols(tibble::as_tibble(d))
  expect_true(all(purrr::map_lgl(out, is.Date)))
  expect_equal(dim(out), c(2, 6))

  out <- convert_date_cols(tibble::as_tibble(dt))
  expect_true(all(purrr::map_lgl(out, is.POSIXt)))
  expect_equal(dim(out), c(2, 6))
})

test_that("Mixed data frame converts all date columns", {
  out <- convert_date_cols(d_mixed)

  # check type on all but date time column
  expect_equal(as.character(purrr::map_chr(select(out, -f_DTS), class)),
               c("Date", "numeric", "Date", "character", "Date"))

  # check type on date time column
  expect_equal(as.character(class(out$f_DTS)), c("POSIXct", "POSIXt"))

  # check converted contents
  expect_equal(as.character(out$z_DTS),
               c("2018-03-05", "2018-03-26", "2019-06-05"))
  expect_equal(as.character(out$c_DTS),
               c("2018-03-05", "2018-03-26", "2019-06-05"))
  expect_equal(as.character(out$e_date),
               c("2018-03-05", "2018-03-26", "2019-06-05"))
  expect_equal(as.character(out$f_DTS),
               c("2000-01-01 13:30:05", "2000-01-10 01:50:10", "2001-05-10 12:50:10"))
})

test_that("Mixed tibble converts all date columns", {
  out <- convert_date_cols(tibble::as_tibble(d_mixed))
  expect_equal(as.character(purrr::map_chr(select(out, -f_DTS), class)),
               c("Date", "numeric", "Date", "character", "Date"))
  expect_equal(as.character(class(out$f_DTS)), c("POSIXct", "POSIXt"))
})
