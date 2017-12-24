context("Testing impute")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build data set to predict whether or not animal_id is a kitty
n <- 300
df <- data.frame(animal_id = 1:n,
                 length = rnorm(n, mean = 7, sd = 2),
                 width = rnorm(n, mean = 2, sd = 0.5),
                 fur = sample(c("Long", "Short"), size = n, replace = T),
                 color = sample(c("Orange", "Black", "White", "Mixed"),
                                    size = n, replace = T)
)

# give kitty likeliness score
df["kitty"] <- df["length"] - 2 * df["width"] - 1
df$kitty[df["fur"] == "Long"]  <-
  df$kitty[df["fur"] == "Long"] + 1
df$kitty[df["fur"] == "Short"]  <-
  df$kitty[df["fur"] == "Short"] - 1
df$kitty[df["color"] == "Mixed"] <-
  df$kitty[df["color"] == "Mixed"] + 1
df$kitty[df["color"] == "Orange"] <-
  df$kitty[df["color"] == "Orange"] + 2
df$kitty[df["color"] == "Black"] <-
  df$kitty[df["color"] == "Black"] - 1
df$kitty[df["color"] == "White"] <-
  df$kitty[df["color"] == "White"] - 4


# Add noise
df$kitty <- df$kitty + rnorm(n, mean = 0, sd = 1.25)
df$kitty <- ifelse(df$kitty > 0, "Y", "N")

# Add missing data
df$color[sample(1:n, 32, replace = FALSE)] <- NA
df$length[sample(1:n, 51, replace = FALSE)] <- NA
df$fur[sample(1:n, 125, replace = FALSE)] <- NA
df$width[sample(1:n, 9, replace = FALSE)] <- NA

train_index <- caret::createDataPartition(
  df$kitty,
  p = 0.8,
  times = 1,
  list = TRUE)

d_train <- df[train_index$Resample1, ]
d_test <- df[-train_index$Resample1, ]

d_train$length[1] <- d_test$length[1] <- NA
d_train$color[2] <- d_test$color[2] <- NA
d_train$width[3] <-  d_test$width[3] <- NA
d_train$fur[3] <- d_test$fur[3] <- NA


# Tests ------------------------------------------------------------------------
test_that("Bad data throws an error", {
  expect_error(impute(),
               regexp = "\"data\" must be a tibble")
  expect_error(impute(data = "yeah hi!"),
               regexp = "\"data\" must be a tibble")
  expect_error(capture_output(
    impute(data = df, grain = "non-column", target = "kitty")),
           regexp = "\"grain\" must be a column name in data")
  expect_error(capture_output(
    impute(data = df, grain = "animalID", target = "naught")),
           regexp = "\"target\" must be a column name in data")
  expect_error(impute(data = df, grain = "animalID", target = "kitty",
                      rec_obj = "fried_fish"),
               regexp = "\"rec_obj\" must be a valid recipe object.")

})

test_that("No recipe with defaults trains and predicts.", {
  capture_output(res <- impute(data = d_train,
                               grain = "animal_id",
                               target = "kitty"))
  expect_equal(res$data_imputed$length[1], 7.1, tol = .02)
  expect_equal(as.character(res$data_imputed$color[2]), "hcai_missing")
  expect_equal(as.character(res$data_imputed$fur[3]), "hcai_missing")
  expect_equal(res$data_imputed$width[3], 2.02, tol = .02)

  capture_output(res <- impute(data = d_test,
                               grain = "animal_id",
                               target = "kitty",
                               rec_obj = res$rec_obj))
  expect_equal(res$data_imputed$length[1], 7.1, tol = .02)
  expect_equal(as.character(res$data_imputed$color[2]), "hcai_missing")
  expect_equal(as.character(res$data_imputed$fur[3]), "hcai_missing")
  expect_equal(res$data_imputed$width[3], 2.02, tol = .02)
})

test_that("No recipe with methods trains and predicts.", {
  capture_output(res <- impute(data = d_train,
                               grain = "animal_id",
                               target = "kitty",
                               nominal_method = "bagimpute",
                               numeric_method = "knnimpute"))
  expect_equal(res$data_imputed$length[1], 6.6, tol = .02)
  expect_equal(as.character(res$data_imputed$color[2]), "Black")
  expect_equal(as.character(res$data_imputed$fur[3]), "Short")
  expect_equal(res$data_imputed$width[3], 1.73, tol = .02)

  capture_output(res <- impute(data = d_test,
                               grain = "animal_id",
                               target = "kitty",
                               rec_obj = res$rec_obj))
  expect_equal(res$data_imputed$length[1], 4.78, tol = .02)
  expect_equal(as.character(res$data_imputed$color[2]), "Mixed")
  expect_equal(as.character(res$data_imputed$fur[3]), "Long")
  expect_equal(res$data_imputed$width[3], 1.95, tol = .02)
})

test_that("No recipe with methods and params trains and predicts.", {
  capture_output(res <- impute(data = d_train,
                               grain = "animal_id",
                               target = "kitty",
                               nominal_method = "bagimpute",
                               numeric_method = "knnimpute",
                               nominal_params =
                                 list(bag_options = list(nbagg = 20)),
                               numeric_params = list(knn_K = 3)))
  expect_equal(res$data_imputed$length[1], 7.1, tol = .02)
  expect_equal(as.character(res$data_imputed$color[2]), "Black")
  expect_equal(as.character(res$data_imputed$fur[3]), "Short")
  expect_equal(res$data_imputed$width[3], 1.83, tol = .02)

  capture_output(res <- impute(data = d_test,
                               grain = "animal_id",
                               target = "kitty",
                               rec_obj = res$rec_obj))
  expect_equal(res$data_imputed$length[1], 4.68, tol = .02)
  expect_equal(as.character(res$data_imputed$color[2]), "Mixed")
  expect_equal(as.character(res$data_imputed$fur[3]), "Short")
  expect_equal(res$data_imputed$width[3], 1.95, tol = .02)
})

test_that("Grain and target are not imputed but are returned.", {
  d_train$animal_id[1:5] <- NA
  d_train$kitty[1:5] <- NA
  capture_output(res <- impute(data = d_train,
                               grain = "animal_id",
                               target = "kitty"))
  expect_true(is.na(res$data_imputed$animal_id[2]))
  expect_true(is.na(res$data_imputed$kitty[4]))

  d_test$animal_id[1:5] <- NA
  d_test$kitty[1:5] <- NA
  capture_output(res <- impute(data = d_test,
                               grain = "animal_id",
                               target = "kitty",
                               rec_obj = res$rec_obj))
  expect_true(is.na(res$data_imputed$animal_id[2]))
  expect_true(is.na(res$data_imputed$kitty[4]))
})

test_that("Columns have the same order after", {
  capture_output(res <- impute(data = d_train,
                               grain = "animal_id",
                               target = "kitty"))
  expect_equal(names(d_train), names(res$data_imputed))
})
