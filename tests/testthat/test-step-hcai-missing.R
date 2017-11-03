
context("Checking recipe step hcai-missing")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)

# build hot dog set
n = 300
d <- tibble(id = 1:n,
                 playerID = sample(1:9, size = n, replace = TRUE),
                 level = sample(1:12, size = n, replace = TRUE),
                 world = sample(1:8, size = n, replace = TRUE), 
                 character = sample(c("Mario", "Luigi"), size = n, 
                                    replace = TRUE),
                 suit = sample(c("Fire Flower", "Raccoon", "Frog", "P Wing"), 
                                    size = n, replace = TRUE)
                 )

# target
d["is_goomba"] <- ifelse((d["world"] - 2*d["level"] - 1) > 0, "Y", "N")

# Add NAs
inds <- sample(1:n, 30, replace = FALSE)
d$suit[inds] <- NA
inds <- sample(1:n, 100, replace = FALSE)
d$character[inds] <- NA

train_index <- caret::createDataPartition(
  d$is_goomba,
  p = 0.8,
  times = 1,
  list = TRUE)

d_train <- d[train_index$Resample1, ]
d_test <- d[-train_index$Resample1, ]

# Tests ------------------------------------------------------------------------
test_that("Recip", {

  
  # Check that a prediction is made
  expect_equal(rfOutd1$id[1], 9001)
  expect_is(rfOutd1$PredictedProbNBR[1], "numeric")
})



rec_obj <- recipe(is_goomba ~ ., data = d)

rec_obj <- rec_obj %>%
  step_hcai_missing(all_nominal())

data_train <- data[train_index$Resample1, ]
data_test <- data[-train_index$Resample1, ]
rec_obj <- prep(rec_obj, training = data_train)

out <- bake(rec_obj, data_train)
out