context("Testing convert_date_cols and step_date_hcai")

# Setup ------------------------------------------------------------------------
d <- data.frame(a_DTS = c("2018-3-25", "2018-3-26"),
                b_DTS = c("2018-03-25", "2018-03-26"),
                c_DTS = c("3-25-2018", "3-26-2018"),
                d_DTS = c("03-25-2018", "03-26-2018"),
                e_DTS = c("Mar 25 2018", "Mar 26 2018"),
                f_DTS = c("March 25th, 2018", "March 26th, 2018"),
                aa_DTS = c("2018-3-25 22:30:00",  "2018-3-26 22:30:00"),
                bb_DTS = c("2018-03-25 22:30:00",  "2018-03-26 22:30:00"),
                cc_DTS = c("3-25-2018 22:30:00",  "3-26-2018 22:30:00"),
                dd_DTS = c("03-25-2018 22:30:00",  "03-26-2018 22:30:00"),
                ee_DTS = c("Mar 25 2018 22:30:00",  "Mar 26 2018 22:30:00"),
                ff_DTS = c("March 25th, 2018 22:30:00",
                           "March 26th, 2018 22:30:00"),
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
                                     "Marches 26th, 2018 22:30:00"))
  expect_error(convert_date_cols(d), "not_DTS")
})

test_that("all common formats are converted in tibble", {
  out <- convert_date_cols(tibble::as_tibble(d))
  expect_true(all(purrr::map(out, class) == "Date"))
  expect_equal(dim(out), c(2, 12))
})

# Setup ------------------------------------------------------------------------
d <- data.frame(z_DTS = c("2018-3-5", "2018-3-26", "2019-6-5"),
                b_nums = c(2, 4, 6),
                c_DTS = c("03-05-2018", "03-26-2018", "06-05-2019"),
                d_chars = c("a", "b", "d"),
                e_date = lubridate::mdy(c("3-05-2018", "3-26-2018",
                                          "06-05-2019")),
                stringsAsFactors = FALSE)

# Tests ------------------------------------------------------------------------
test_that("Mixed data frame converts all date columns", {
  out <- convert_date_cols(d)
  expect_equal(as.character(purrr::map_chr(out, class)),
               c("Date", "numeric", "Date", "character", "Date"))
  expect_equal(as.character(out$z_DTS),
               c("2018-03-05", "2018-03-26", "2019-06-05"))
  expect_equal(as.character(out$c_DTS),
               c("2018-03-05", "2018-03-26", "2019-06-05"))
  expect_equal(as.character(out$e_date),
               c("2018-03-05", "2018-03-26", "2019-06-05"))
})

test_that("Mixed tibble converts all date columns", {
  out <- convert_date_cols(tibble::as_tibble(d))
  expect_equal(as.character(purrr::map_chr(out, class)),
               c("Date", "numeric", "Date", "character", "Date"))
})

test_that("same results as recipes::step_date for dow, month, and year", {
  d <- d %>% dplyr::select(e_date, b_nums, d_chars)
  cols <- find_date_cols(d)
  sdf <- c("dow", "month", "year")

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- step_date_hcai(date_rec, cols = cols, features = "categories") %>%
    recipes::step_rm(cols = cols)
  date_rec <- recipes::prep(date_rec, training = d)
  d_hcai <- recipes::bake(date_rec, newdata = d)
  d_hcai <- select(d_hcai, contains("date")) %>% select(-contains("hour"))

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- recipes::step_date(date_rec, cols = cols, features = sdf) %>%
    recipes::step_rm(cols = cols)
  date_rec <- recipes::prep(date_rec, training = d)
  d_recipes <- recipes::bake(date_rec, newdata = d)
  d_recipes <- select(d_recipes, contains("date"))

  expect_equal(d_hcai, d_recipes)
})

test_that("Print method works correctly", {
  d <- d %>% dplyr::select(e_date, b_nums, d_chars)
  cols <- find_date_cols(d)
  sdf <- c("dow", "month", "year")

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- step_date_hcai(date_rec, cols = cols, features = "categories")
  date_rec <- recipes::prep(date_rec, training = d)

  expect_output(
    print(date_rec),
    regexp = "Date features from e_date"
  )
})

test_that("tidy method prints correctly for categories features", {
  d <- d %>% dplyr::select(e_date, b_nums, d_chars)
  cols <- find_date_cols(d)

  date_rec <-
    recipes::recipe(head(d), ~ .) %>%
    step_date_hcai(cols = cols, features = "categories") %>%
    recipes::prep(training = d)

  exp <- tibble::as_tibble(
    data.frame(
      terms = "e_date",
      value = factor(c("hour", "dow", "month", "year"),
                     levels = c("hour", "dow", "month", "year")),
      features = "categories"
    )
  )

  expect_equal(
    exp,
    broom::tidy(date_rec$steps[[1]]))
  expect_s3_class(broom::tidy(date_rec$steps[[1]]), "tbl_df")


})

test_that("tidy method prints correctly for continuous features", {
  d <- d %>% dplyr::select(e_date, b_nums, d_chars)
  cols <- find_date_cols(d)

  date_rec <-
    recipes::recipe(head(d), ~ .) %>%
    step_date_hcai(cols = cols, features = "continuous") %>%
    recipes::prep(training = d)

  exp <- tibble::as_tibble(
    data.frame(
      terms = "e_date",
      value = factor(c("hour_sin", "hour_cos", "dow_sin", "dow_cos", "month_sin", "month_cos", "year"),
                     levels = c("hour_sin", "hour_cos", "dow_sin", "dow_cos", "month_sin", "month_cos", "year")),
      features = "continuous"
    )
  )

  expect_equal(
    exp,
    broom::tidy(date_rec$steps[[1]]))
  expect_s3_class(broom::tidy(date_rec$steps[[1]]), "tbl_df")
})

## Things to test
# 1. hours are not being created unless they need to.
# 2. find another way to test hours

