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
               regexp = "\"d\" must be a tibble")
  expect_error(impute(d = "yeah hi!"),
               regexp = "\"d\" must be a tibble")
  expect_error(impute(d = df, recipe = "fried_fish"),
               regexp = "\"recipe\" must be a valid recipe object.")
})

test_that("Bad ignore_colums are parsed correctly.", {
  capture_output(expect_error(impute(d = d_train, chippies),
                              regexp = "chippies not found in d"))
  capture_output(expect_error(impute(d = d_train, chippies, fishes),
                              regexp = "chippies, fishes not found in d"))
})

test_that("No recipe with defaults trains and predicts.", {
  capture_output(res <- impute(d = d_train,
                               animal_id, kitty))
  expect_equal(res$length[1], 7.1, tol = .02)
  expect_equal(as.character(res$color[2]), "missing")
  expect_equal(as.character(res$fur[3]), "missing")
  expect_equal(res$width[3], 2.02, tol = .02)

  capture_output(res <- impute(d = d_test,
                               animal_id, kitty,
                               recipe = attr(res, "recipe")))
  expect_equal(res$length[1], 7.1, tol = .02)
  expect_equal(as.character(res$color[2]), "missing")
  expect_equal(as.character(res$fur[3]), "missing")
  expect_equal(res$width[3], 2.02, tol = .02)
})

test_that("No recipe with methods trains and predicts.", {
  capture_output(res <- impute(d = d_train,
                               animal_id, kitty,
                               nominal_method = "bagimpute",
                               numeric_method = "knnimpute"))
  expect_equal(res$length[1], 6.6, tol = .02)
  expect_equal(as.character(res$color[2]), "Black")
  expect_equal(as.character(res$fur[3]), "Short")
  expect_equal(res$width[3], 1.73, tol = .02)

  capture_output(res <- impute(d = d_test,
                               animal_id, kitty,
                               recipe = attr(res, "recipe")))
  expect_equal(res$length[1], 4.78, tol = .02)
  expect_equal(as.character(res$color[2]), "Mixed")
  expect_equal(as.character(res$fur[3]), "Long")
  expect_equal(res$width[3], 1.95, tol = .02)
})

test_that("No recipe with methods and params trains and predicts.", {
  capture_output(res <- impute(d = d_train,
                               animal_id, kitty,
                               nominal_method = "bagimpute",
                               numeric_method = "knnimpute",
                               nominal_params =
                                 list(bag_options = list(nbagg = 20)),
                               numeric_params = list(knn_K = 3)))
  expect_equal(res$length[1], 7.1, tol = .02)
  expect_equal(as.character(res$color[2]), "Black")
  expect_equal(as.character(res$fur[3]), "Short")
  expect_equal(res$width[3], 1.83, tol = .02)

  capture_output(res <- impute(d = d_test,
                               animal_id, kitty,
                               recipe = attr(res, "recipe")))
  expect_equal(res$length[1], 4.68, tol = .02)
  expect_equal(as.character(res$color[2]), "Mixed")
  expect_equal(as.character(res$fur[3]), "Short")
  expect_equal(res$width[3], 1.95, tol = .02)
})

test_that("Ignored columns are not imputed but are returned.", {
  d_train$animal_id[1:5] <- NA
  d_train$kitty[1:5] <- NA
  expect_warning(capture_output(res <- impute(d = d_train, animal_id, kitty)))
  expect_true(is.na(res$animal_id[2]))
  expect_true(is.na(res$kitty[4]))

  d_test$animal_id[1:5] <- NA
  d_test$kitty[1:5] <- NA
  expect_warning(capture_output(
    res <- impute(d = d_test, animal_id, kitty, recipe = attr(res, "recipe"))
  ))
  expect_true(is.na(res$animal_id[2]))
  expect_true(is.na(res$kitty[4]))
})

test_that("Columns have the same order after", {
  capture_output(res <- impute(d = d_train, animal_id, kitty))
  expect_equal(names(d_train), names(res))
})

test_that("Missingness in ignored columns throws warning, elsewhere doesn't", {
  d_train$animal_id[1:10] <- NA
  expect_warning(
    capture_output(tmp <- impute(d_train, animal_id)))
  expect_warning(capture_output(
    tmp <- impute(d_train, kitty)), regexp = NA)
})

test_that("Output of impute is a data frame with our custom child class", {
  capture_output(imped <- impute(d_train))
  expect_true(is.data.frame(imped))
  expect_s3_class(imped, "hcai_imputed_df")
})

test_that("Output of impute is same for tibble vs data frame", {
  expect_equal(
    capture_output(impute(d_train)),
    capture_output(impute(tibble::as_tibble(d_train)))
  )
})

test_that("recipe attr is a recipe class object", {
  capture_output(imp_train <- impute(d_train))
  expect_true("recipe" %in% names(attributes(imp_train)))
  expect_s3_class(attr(imp_train, "recipe"), "recipe")
})

test_that("imp_summary attr is contained within d_imputed", {
  capture_output(imp_train <- impute(d_train))
  expect_true("imp_summary" %in% names(attributes(imp_train)))
})

test_that("print method works as expected", {
  d_train$animal_id[1:5] <- NA
  d_train$kitty[1:5] <- NA
  expect_warning(
    msg <- capture_output(
      capture_messages(
        res <- impute(d = d_train, animal_id, kitty, verbose = TRUE))))
  expect_true(grepl("ignored", msg))
  expect_true(grepl("new_category", msg))
})

test_that("a data.frame with a recipe in recipe slot works", {
  imp_train <- impute(d_train)
  expect_equal(impute(d_test, recipe = imp_train),
               impute(d_test, recipe = attr(imp_train, "recipe")))
})

test_that("an attr that doesn't exist passed to recipe errors", {
  imp_train <- impute(d_train)
  expect_error(impute(d_test, recipe = attr(imp_train, "nonsense")),
               regexp = "nonsense")
})
