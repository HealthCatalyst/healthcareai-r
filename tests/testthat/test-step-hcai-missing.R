context("Checking recipe step hcai-missing")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
browser()
# build dataset
n <- 300
d <- tibble::tibble(id = 1:n,
                 playerID = sample(1:9, size = n, replace = TRUE),
                 level = sample(1:12, size = n, replace = TRUE),
                 world = sample(1:8, size = n, replace = TRUE),
                 character = sample(c("Mario", "Luigi"), size = n,
                                    replace = TRUE),
                 suit = sample(c("Fire Flower", "Raccoon", "Frog", "P Wing"),
                                    size = n, replace = TRUE)
                 )

# target
d["is_goomba"] <- ifelse( (d["world"] - 2 * d["level"] - 1) > 0, "Y", "N")

# Add NAs
inds <- sample(n, 30, replace = FALSE)
d$suit[inds] <- NA
inds <- sample(n, 100, replace = FALSE)
d$character[inds] <- NA

train_index <- caret::createDataPartition(
  d$is_goomba,
  p = 0.8,
  times = 1,
  list = TRUE)

d_train <- d[train_index$Resample1, ]
d_test <- d[-train_index$Resample1, ]

rec_obj <- recipes::recipe(is_goomba ~ ., data = d)

rec_obj <- rec_obj %>%
  step_hcai_missing(all_nominal())

junk <- capture.output(
  rec_obj <- recipes::prep(rec_obj, training = d_train)
)

junk <- capture.output(
  out_train <- recipes::bake(rec_obj, d_train)
)

junk <- capture.output(
  out_test <- recipes::bake(rec_obj, d_test)
)


d$koopa = sample(c("Blue", "Red", NA), prob = c(.2, .212, .588), 
                  size = n, replace = TRUE)
d_train <- d[train_index$Resample1, ]

rec_obj2 <- recipe(is_goomba ~ ., data = d) %>%
  step_hcai_missing(matches("koopa"))

mes <- capture.output(
  junk <- capture.output(
    junk <- recipes::prep(rec_obj2, training = d2_train)
  ), type = "message"
)

# Tests ------------------------------------------------------------------------
test_that("Recipe object is updated with step", {
  expect_equal(class(rec_obj$steps[[1]])[1], "step_hcai_missing")
})

test_that("Recipe is prepped correctly", {
  expect_equal(
    rec_obj$steps[[1]]$na_percentage[[1]],
    33.2)

  expect_equal(
    names(rec_obj$steps[[1]]$na_percentage)[1],
    "character")

  expect_equal(
    rec_obj$steps[[1]]$na_percentage[[2]],
    8.7)

  expect_equal(
    names(rec_obj$steps[[1]]$na_percentage)[2],
    "suit")
})

test_that("Recipe is baked correctly on training data", {
  expect_true("hcai_missing" %in% levels(out_train$character))

  expect_equal(
    sum(out_train$character == "hcai_missing"),
    80)

  expect_true("hcai_missing" %in% levels(out_train$suit))

  expect_equal(
    sum(out_train$suit == "hcai_missing"),
    21)
})

test_that("Recipe is baked correctly on test data", {
  expect_true("hcai_missing" %in% levels(out_test$character))

  expect_equal(
    sum(out_test$character == "hcai_missing"),
    20)

  expect_true("hcai_missing" %in% levels(out_test$suit))

  expect_equal(
    sum(out_test$suit == "hcai_missing"),
    9)
})

test_that("Printer method works correctly within print.recipe()", {
  res <- capture.output(recipes:::print.recipe(rec_obj))
  expect_equal(
    res[13],
    "Filling NA with hcai_missing for character, suit [trained]")
})

test_that("Warning is triggered for greater than 50% NA", {
  expect_equal(
    mes[3],
    "koopa: 61%")
})

