context("Testing impute")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build hot dog set
n <- 300
df <- data.frame(id = 1:n,
                 animalID = sample(1:9, size = n, replace = T),
                 length = rnorm(n, mean = 7, sd = 2),
                 width = rnorm(n, mean = 2, sd = 0.5),
                 fur = sample(c("Long", "Short"), size = n, replace = T),
                 color = sample(c("Orange", "Black", "White", "Mixed"),
                                    size = n, replace = T)
)

# give kitty likeliness score
df["kitty"] <- df["length"] - 2 * df["width"] - 1
df$kitty[df["fur"] == "Hot"]  <-
  df$kitty[df["fur"] == "Hot"] + 1
df$kitty[df["fur"] == "Cold"]  <-
  df$kitty[df["fur"] == "Cold"] - 1
df$kitty[df["color"] == "Ketchup"] <-
  df$kitty[df["color"] == "Ketchup"] + 1
df$kitty[df["color"] == "Mustard"] <-
  df$kitty[df["color"] == "Mustard"] + 2
df$kitty[df["color"] == "Wasabi"] <-
  df$kitty[df["color"] == "Wasabi"] - 1
df$kitty[df["color"] == "Syrup"] <-
  df$kitty[df["color"] == "Syrup"] - 4


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

rec_obj <- recipe(kitty ~ ., data = d_train)

# Tests ------------------------------------------------------------------------
test_that("Bad data throws an error", {
  expect_error(impute(),
               regexp = "\"data\" must be a tibble")
  expect_error(impute(data = "yeah hi!"),
               regexp = "\"data\" must be a tibble")
})
