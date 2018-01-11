context("Testing data_prep")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build data set to predict whether or not animal_id is a is_ween
n <- 300
df <- data.frame(song_id = 1:n,
        length = rnorm(n, mean = 4, sd = 1),
        weirdness = rnorm(n, mean = 4, sd = 2),
        genre = sample(c("Rock", "Jazz", "Country"),
                       size = n, replace = T),
        reaction = sample(c("Love", "Huh", "Dislike", "Mixed"),
                          size = n, replace = T),
        guitar_flag = sample(c(0, 1), size = n, replace = T),
        drum_flag = sample(c(0, 1, NA), size = n, replace = T,
                           prob = c(0.45, 0.45, 0.1)),
        date_col = lubridate::ymd("2002-03-04") + lubridate::days(1:10),
        posixct_col = lubridate::ymd("2004-03-04") + lubridate::days(1:10),
        col_DTS = lubridate::ymd("2006-03-04") + lubridate::days(1:10),
        missing82 = sample(1:10, n, replace = TRUE),
        missing64 = sample(100:300, n, replace = TRUE),
        state = sample(c("NY", "MA", "CT", "CA", "VT", "NH"), size = n, replace = T,
                           prob = c(0.18, 0.1, 0.1, 0.6, 0.01, 0.01)),
        a_nzv_col = sample(c("man", "boognish"),
                           size = n, replace = T, prob = c(0.999, 0.001))
)

# give is_ween likeliness score
df["is_ween"] <- df["length"] - 1 * df["weirdness"] + 2
df$is_ween[df["genre"] == "Rock"]  <-
  df$is_ween[df["genre"] == "Rock"] + 2
df$is_ween[df["genre"] == "Jazz"]  <-
  df$is_ween[df["genre"] == "Jazz"] - 1
df$is_ween[df["genre"] == "Country"]  <-
  df$is_ween[df["genre"] == "Country"] - 1
df$is_ween[df["reaction"] == "Huh"] <-
  df$is_ween[df["reaction"] == "Huh"] + 1
df$is_ween[df["reaction"] == "Love"] <-
  df$is_ween[df["reaction"] == "Love"] + 2
df$is_ween[df["reaction"] == "Mixed"] <-
  df$is_ween[df["reaction"] == "Mixed"] - 1
df$is_ween[df["reaction"] == "Dislike"] <-
  df$is_ween[df["reaction"] == "Dislike"] - 4


# Add noise
df$is_ween <- df$is_ween + rnorm(n, mean = 0, sd = 1.25)
df$is_ween <- ifelse(df$is_ween > 0, "Y", "N")

# Add missing data
df$reaction[sample(1:n, 32, replace = FALSE)] <- NA
df$length[sample(1:n, 51, replace = FALSE)] <- NA
df$genre[sample(1:n, 125, replace = FALSE)] <- NA
df$weirdness[sample(1:n, 9, replace = FALSE)] <- NA

train_index <- caret::createDataPartition(
  df$is_ween,
  p = 0.8,
  times = 1,
  list = TRUE)

d_train <- df[train_index$Resample1, ]
d_test <- df[-train_index$Resample1, ]

d_train$length[1] <- d_test$length[1] <- NA
d_train$reaction[2] <- d_test$reaction[2] <- NA
d_train$weirdness[3] <-  d_test$weirdness[3] <- NA
d_train$genre[3] <- d_test$genre[3] <- NA


# Tests ------------------------------------------------------------------------
test_that("Bad data throws an error", {
  expect_error(data_prep(),
               regexp = "\"d\" must be a tibble")
  expect_error(data_prep(d = "yeah hi!"),
               regexp = "\"d\" must be a tibble")
})

test_that("Bad target, grain, or ignore throws an error", {
  expect_error(data_prep(d = d_train),
               regexp = "\"target\" is required")
  expect_error(data_prep(d = d_train, target = is_ween),
               regexp = "\"grain\" is required")
  expect_error(data_prep(d = d_train, target = is_queen, grain = song_id),
               regexp = "not found in d")
  expect_error(data_prep(d = d_train, target = is_ween, grain = dance_id),
               regexp = "not found in d")
  expect_error(data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id,
                         cowbell),
               regexp = "not found in d")
  expect_error(data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id,
                         cowbell, spoons),
               regexp = "cowbell, spoons not found in d")
})

test_that("0/1 columns are found and converted with defaults", {
  capture_output(
    d_clean <- data_prep(d = d_train,
                       target = is_ween,
                       grain = song_id)
  )

  expect_true(is.factor(d_clean$guitar_flag))
  expect_true(is.factor(d_clean$drum_flag))

  lev <- levels(d_clean$guitar_flag)
  expect_equal(lev[ordered(lev)], c("no", "yes"))

  lev <- levels(d_clean$drum_flag)
  expect_equal(lev[ordered(lev)], c("no", "yes"))
})

test_that("date columns are found and converted with defaults", {
  capture_output(
    d_clean <- data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id,
                         center = FALSE,
                         scale = FALSE)
  )

  expect_true(is.factor(d_clean$date_col_dow))
  expect_true(is.factor(d_clean$posixct_col_month))
  expect_true(is.numeric(d_clean$col_DTS_year))
  expect_equal(as.character(d_clean$date_col_month[1]), "Mar")
  expect_equal(d_clean$posixct_col_year[2], 2004)
  expect_equal(as.character(d_clean$col_DTS_dow[3]), "Tues")
})

test_that("impute works with defaults", {
  capture_output(
    d_clean <- data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id,
                         center = FALSE,
                         scale = FALSE)
  )
  expect_equal(d_clean$weirdness[3], 3.88, tol = .01)
  expect_equal(as.character(d_clean$genre[2]), "Country")
  expect_equal(as.character(d_clean$reaction[4]), "Dislike")
})

test_that("impute works with params", {
  capture_output(
    d_clean <- data_prep(d = d_train,
                 target = is_ween,
                 grain = song_id,
                 center = FALSE,
                 scale = FALSE,
                 impute = list(numeric_method = "knnimpute",
                               nominal_method = "bagimpute",
                               numeric_params = list(knn_K = 5),
                               nominal_params = NULL))
  )
  expect_equal(d_clean$weirdness[3], 4.55, tol = .01)
  expect_equal(as.character(d_clean$genre[2]), "Country")
  expect_equal(as.character(d_clean$reaction[4]), "Dislike")
})

test_that("impute works with partial/extra params", {
  capture_output(
    d_clean <- data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id,
                         center = FALSE,
                         scale = FALSE,
                         impute = list(numeric_method = "bagimpute"))
  )
  m <- missingness(d_clean)
  expect_equal(m$percent_missing[m$variable == "length"], 18.3)
  expect_true(all(m$percent_missing[!(m$variable %in% "length")] == 0))

  expect_warning(capture_output(
    d_clean <- data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id,
                         center = FALSE,
                         scale = FALSE,
                         impute = list(numeric_method = "knnimpute",
                                       numeric_params = list(knn_K = 5),
                                       the_best_params = "Moi!"))),
  regexp = "the_best_params")
})


test_that("rare factors go to other", {
  capture_output(
    d_clean <- data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id)
  )
  exp <- c("CA", "CT", "MA", "NY", "other")
  expect_equal(levels(d_clean$state), exp)
  exp <- c("Dislike", "Huh", "Love", "Mixed")
  expect_equal(levels(d_clean$reaction), exp)
})

test_that("centering and scaling work", {
  capture_output(
    d_clean <- data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id)
  )
  expect_equal(mean(d_clean$length), 0, tol = .01)
  expect_equal(mean(d_clean$weirdness), 0, tol = .01)
  expect_equal(sd(d_clean$length), 1, tol = .01)
  expect_equal(sd(d_clean$weirdness), 1, tol = .01)
})

test_that("near zero variance columns are removed", {
  capture_output(
    d_clean <- data_prep(d = d_train,
                         target = is_ween,
                         grain = song_id)
  )
  expect_true(is.null(d_clean$a_nzv_col))
})
