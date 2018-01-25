context("Testing data_prep utilities")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build data set to predict whether or not animal_id is a is_ween
n <- 100
d <- data.frame(song_id = 1:n,
       length = rnorm(n, mean = 4, sd = 1),
       tuba_flag = sample(c(0, 1), size = n, replace = T),
       drum_flag = sample(c(0, 1, NA), size = n, replace = T),
       reaction = sample(c("Love", "Huh", "Dislike", "Mixed"),
                         size = n, replace = T),
       date_col = lubridate::ymd("2002-03-04") + lubridate::days(1:10),
       posixct_col = lubridate::ymd("2004-03-04") + lubridate::days(1:10),
       col_DTS = lubridate::ymd("2006-03-04") + lubridate::days(1:10),
       missing82 = sample(1:10, n, replace = TRUE),
       missing64 = sample(100:300, n, replace = TRUE)
)
d$posixct_col <- as.POSIXct(d$posixct_col)
d$col_DTS <- as.character(d$col_DTS)
d$missing82[sample(1:n, 82, replace = FALSE)] <- NA
d$missing64[sample(1:n, 64, replace = FALSE)] <- NA

# Tests ------------------------------------------------------------------------
test_that("find_0_1_cols returns numeric flag columns", {
  exp <- c("tuba_flag", "drum_flag")
  expect_equal(find_0_1_cols(d), exp)
})

test_that("find_mostly_missing_cols returns numeric flag columns", {
  expect_equal(find_mostly_missing_cols(d, 80), "missing82")

  exp <- c("missing64", "missing82")
  expect_equal(find_mostly_missing_cols(d, 60), exp)
})

test_that("find_date_cols returns date columns", {
  exp <- c("date_col", "posixct_col", "col_DTS")
  expect_equal(find_date_cols(d), exp)
})

test_that("check_rec_obj", {
  expect_s3_class(check_rec_obj(recipes::recipe(d)), "recipe")
  expect_s3_class(
    check_rec_obj(structure(d, "rec_obj" = recipes::recipe(d, drum_flag ~ .))),
    "recipe")
  expect_error(check_rec_obj(d))
  expect_error(check_rec_obj("recipe"))
})
