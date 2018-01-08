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
                                    size = n, replace = T)
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
d_train$width[3] <-  d_test$weirdness[3] <- NA
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
