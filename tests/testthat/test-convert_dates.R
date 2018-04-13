context("Testing convert_date_cols")

# Setup ------------------------------------------------------------------------
d <- data.frame(a_DTS = c("2018-3-25", "2018-3-25"),
                b_DTS = c("2018-03-25", "2018-03-25"),
                c_DTS = c("3-25-2018", "3-25-2018"),
                d_DTS = c("03-25-2018", "03-25-2018"),
                e_DTS = c("Mar 25 2018", "Mar 25 2018"),
                f_DTS = c("March 25th, 2018", "March 25th, 2018"),
                aa_DTS = c("2018-3-25 22:30:00",  "2018-3-25 22:30:00"),
                bb_DTS = c("2018-03-25 22:30:00",  "2018-03-25 22:30:00"),
                cc_DTS = c("3-25-2018 22:30:00",  "3-25-2018 22:30:00"),
                dd_DTS = c("03-25-2018 22:30:00",  "03-25-2018 22:30:00"),
                ee_DTS = c("Mar 25 2018 22:30:00",  "Mar 25 2018 22:30:00"),
                ff_DTS = c("March 25th, 2018 22:30:00",
                           "March 25th, 2018 22:30:00"),
                stringsAsFactors = FALSE)

# Tests ------------------------------------------------------------------------
test_that("convert dates finds bad DTS columns", {
  d <- dplyr::bind_cols(d,
    not_DTS = c("string cheese", "string cheese"),
    typo_DTS = c("Marches 25th, 2018 22:30:00",
                 "Marches 25th, 2018 22:30:00"))
  expect_error(convert_date_cols(d), "not_DTS")
})

test_that("all common formats are converted", {
  out <- convert_date_cols(d)
  expect_true(all(purrr::map(out, class) == "Date"))
  expect_equal(dim(out), c(2, 12))
})

test_that("convert dates finds bad DTS columns in tibble", {
  d <- dplyr::bind_cols(tibble::as_tibble(d),
                        not_DTS = c("string cheese", "string cheese"),
                        typo_DTS = c("Marches 25th, 2018 22:30:00",
                                     "Marches 25th, 2018 22:30:00"))
  expect_error(convert_date_cols(d), "not_DTS")
})

test_that("all common formats are converted in tibble", {
  out <- convert_date_cols(tibble::as_tibble(d))
  expect_true(all(purrr::map(out, class) == "Date"))
  expect_equal(dim(out), c(2, 12))
})

# Setup ------------------------------------------------------------------------
d <- data.frame(a_DTS = c("2018-3-25", "2018-3-25"),
                b_nums = c(2, 4),
                c_DTS = c("03-25-2018", "03-25-2018"),
                d_chars = c("a", "b"),
                e_date = lubridate::mdy(c("3-25-2018", "3-25-2018")),
                stringsAsFactors = FALSE)

# Tests ------------------------------------------------------------------------
test_that("Mixed data frame converts all date columns", {
  out <- convert_date_cols(d)
  expect_equal(as.character(purrr::map_chr(out, class)),
               c("Date", "numeric", "Date", "character", "Date"))
})

test_that("Mixed tibble converts all date columns", {
  out <- convert_date_cols(tibble::as_tibble(d))
  expect_equal(as.character(purrr::map_chr(out, class)),
               c("Date", "numeric", "Date", "character", "Date"))
})
