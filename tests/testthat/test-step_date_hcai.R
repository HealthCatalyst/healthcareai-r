context("Testing step_date_hcai")

# Setup ------------------------------------------------------------------------
d <- tibble(
  z_DTS = c("2018-3-5", "2018-3-26", "2019-6-5"),
  b_nums = c(2, 4, 6),
  c_DTS = c("03-05-2018", "03-26-2018", "06-05-2019"),
  d_chars = c("a", "b", "d"),
  e_date = lubridate::mdy(c("3-05-2018", "3-26-2018", "06-05-2019")),
  f_DTS = c("2000-1-1 13:30:05", "2000-1-10 1:50:10", "2001-5-10 12:50:10")
)

# Tests ------------------------------------------------------------------------

test_that("categories - check dow, month, and year with recipes::step_date", {
  d <- d %>% dplyr::select(e_date, b_nums, d_chars)
  cols <- find_date_cols(d)
  sdf <- c("dow", "month", "year")

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- step_date_hcai(date_rec, cols, feature_type = "categories") %>%
    recipes::step_rm(cols)
  date_rec <- recipes::prep(date_rec, training = d)
  d_hcai <- recipes::bake(date_rec, new_data = d)

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- recipes::step_date(date_rec, cols, features = sdf) %>%
    recipes::step_rm(cols)
  date_rec <- recipes::prep(date_rec, training = d)
  d_recipes <- recipes::bake(date_rec, new_data = d)

  expect_equal(d_hcai, d_recipes)
})

test_that("categories - check hour column created corectly", {
  expected <- tibble(
    f_DTS_hour = c(13, 1, 12)
  )

  d <- d %>% dplyr::select(b_nums, f_DTS)
  cols <- find_date_cols(d)

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- step_date_hcai(date_rec, cols, feature_type = "categories") %>%
    recipes::step_rm(cols)
  date_rec <- recipes::prep(date_rec, training = d)
  d_hcai <- recipes::bake(date_rec, new_data = d)

  expect_equal(d_hcai$f_DTS_hour, expected$f_DTS_hour)
})

test_that("continuous - check date time column created correctly", {
  expected <- tibble(
    b_nums = d$b_nums,
    f_DTS_dow_sin = sin(2 * pi / 7 * c(7, 2, 5)),
    f_DTS_dow_cos = cos(2 * pi / 7 * c(7, 2, 5)),
    f_DTS_month_sin = sin(2 * pi / 12 * c(1, 1, 5)),
    f_DTS_month_cos = cos(2 * pi / 12 * c(1, 1, 5)),
    f_DTS_year = c(2000, 2000, 2001),
    f_DTS_hour_sin = sin(2 * pi / 24 * c(13, 1, 12)),
    f_DTS_hour_cos = cos(2 * pi / 24 * c(13, 1, 12))
  )

  d <- d %>% dplyr::select(b_nums, f_DTS)
  cols <- find_date_cols(d)

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- step_date_hcai(date_rec, cols, feature_type = "continuous") %>%
    recipes::step_rm(cols)
  date_rec <- recipes::prep(date_rec, training = d)
  d_hcai <- recipes::bake(date_rec, new_data = d)

  expect_equal(d_hcai, expected)
})

test_that("continuous - check date only column created correctly - no hour", {
  expected <- tibble(
    b_nums = d$b_nums,
    c_DTS_dow_sin = sin(2 * pi / 7 * c(2, 2, 4)),
    c_DTS_dow_cos = cos(2 * pi / 7 * c(2, 2, 4)),
    c_DTS_month_sin = sin(2 * pi / 12 * c(3, 3, 6)),
    c_DTS_month_cos = cos(2 * pi / 12 * c(3, 3, 6)),
    c_DTS_year = c(2018, 2018, 2019)
  )

  d <- d %>% dplyr::select(b_nums, c_DTS)
  cols <- find_date_cols(d)

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- step_date_hcai(date_rec, cols, feature_type = "continuous") %>%
    recipes::step_rm(cols)
  date_rec <- recipes::prep(date_rec, training = d)
  d_hcai <- recipes::bake(date_rec, new_data = d)

  expect_equal(tibble(d_hcai), expected)
})

test_that("Print method works correctly", {
  d <- d %>% dplyr::select(e_date, b_nums, d_chars)
  cols <- find_date_cols(d)
  sdf <- c("dow", "month", "year")

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- step_date_hcai(date_rec, cols, feature_type = "categories")
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
    step_date_hcai(cols, feature_type = "categories", id = "id") %>%
    recipes::prep(training = d)

  exp <- tibble::as_tibble(
    tibble(
      terms = "e_date",
      feature_type = "categories",
      id = "id"
    )
  )

  expect_equal(
    as.character(exp),
    as.character(tidy(date_rec$steps[[1]]) %>% unlist(., use.names = FALSE))
  )
  expect_s3_class(tidy(date_rec$steps[[1]]), "tbl_df")
})

test_that("test possible values for `feature_type`", {
  expect_error(
    recipes::recipe(head(d), ~ .) %>%
      step_date_hcai(cols = cols, feature_type = "test"),
    "Possible values"
  )
})

test_that("convert_to_circular converts to circular correctly", {
  expect_equal(convert_to_circular(3, 12, sin), sin(pi / 2))
  expect_equal(convert_to_circular(3, 12, cos), cos(pi / 2))

  expected <- c(cos(pi / 2), cos(pi / 2), cos(pi))
  actual <- convert_to_circular(lubridate::month(d$e_date), 12, cos)
  expect_equal(actual, expected)
})

test_that("ord2fac - tibble", {
  d <- tibble(
    test1 = factor(c("slow", "medium", "fast", "slow"),
                   levels = c("slow", "medium", "fast"), ordered = TRUE)
  )
  actual <- ord2fac(d, "test1")
  expect_false(is.ordered(actual))
})

test_that("ord2fac - df", {
  d <- tibble(
    test1 = factor(c("slow", "medium", "fast", "slow"),
                   levels = c("slow", "medium", "fast"), ordered = TRUE)
  )
  actual <- ord2fac(d, "test1")
  expect_false(is.ordered(actual))
})

test_that("test tidy prints correctly", {
  d <- d %>% dplyr::select(e_date, b_nums, d_chars)
  cols <- find_date_cols(d)
  sdf <- c("dow", "month", "year")

  date_rec <- recipes::recipe(head(d), ~ .)
  date_rec <- step_date_hcai(date_rec, cols, feature_type = "categories",
                             id = "bagimpute_9tNN4") %>%
    recipes::step_rm(cols)

  exp <- tibble(
    terms = as.factor("cols"),
    feature_type = as.factor("categories"),
    id = as.factor("bagimpute_9tNN4")
  )
  expect_equal(
    exp,
    tidy(date_rec$steps[[1]]),
    check.attributes = FALSE
  )
  exp$terms <- as.factor("e_date")
  date_rec <- recipes::prep(date_rec, training = d)
  expect_equal(
    exp,
    tidy(date_rec$steps[[1]]),
    check.attributes = FALSE
  )
})
