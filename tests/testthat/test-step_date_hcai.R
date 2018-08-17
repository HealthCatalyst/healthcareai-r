context("Testing step_date_hcai")

# Setup ------------------------------------------------------------------------
d <- data.frame(
  z_DTS = c("2018-3-5", "2018-3-26", "2019-6-5"),
  b_nums = c(2, 4, 6),
  c_DTS = c("03-05-2018", "03-26-2018", "06-05-2019"),
  d_chars = c("a", "b", "d"),
  e_date = lubridate::mdy(c("3-05-2018", "3-26-2018", "06-05-2019")),
  f_DTS = c("2000-1-1 13:30:05", "2000-1-10 1:50:10", "2001-5-10 12:50:10"),
  stringsAsFactors = FALSE
)

# Tests ------------------------------------------------------------------------

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
