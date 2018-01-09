context("Testing data_prep utilities")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build data set to predict whether or not animal_id is a is_ween
n <- 300
d <- data.frame(song_id = 1:n,
                length = rnorm(n, mean = 4, sd = 1),
                weirdness = sample(c(0, 1), size = n, replace = T),
                genre = sample(c(0, 1, NA), size = n, replace = T),
                reaction = sample(c("Love", "Huh", "Dislike", "Mixed"),
                                  size = n, replace = T),
                date_col = lubridate::ymd("2002-03-04") + days(1:10),
                posixct_col = lubridate::ymd("2004-03-04") + days(1:10),
                col_DTS = lubridate::ymd("2006-03-04") + days(1:10)
)
d$posixct_col <- as.POSIXct(d$posixct_col)
d$col_DTS <- as.character((d$col_DTS))

# Tests ------------------------------------------------------------------------
test_that("find_date_cols returns date columns", {
  exp <- c("date_col", "posixct_col", "col_DTS")

  expect_equal(find_date_cols(d), exp)
})
