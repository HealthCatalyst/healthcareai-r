context("Checking recipe step hcai-missing")

library(recipes)
# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
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
d$is_goomba <- ifelse((d$world - 2 * d$level - 1) > 0, "Y", "N")
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

rec_obj <- recipe(is_goomba ~ ., data = d)

rec_obj <- rec_obj %>%
  step_missing(all_nominal(), id = "id")

junk <- capture_output(
  rec_obj <- prep(rec_obj, training = d_train)
)

junk <- capture_output(
  out_train <- bake(rec_obj, d_train)
)

junk <- capture_output(
  out_test <- bake(rec_obj, d_test)
)


d$koopa <- sample(c("Blue", "Red", NA), prob = c(.2, .212, .588),
                  size = n, replace = TRUE)
d2_train <- d[train_index$Resample1, ]

rec_obj2 <- recipe(is_goomba ~ ., data = d) %>%
  step_missing(starts_with("koop"))

# Tests ------------------------------------------------------------------------
test_that("Recipe object is updated with step", {
  expect_equal(class(rec_obj$steps[[1]])[1], "step_missing")
})

test_that("Recipe is prepped correctly", {
  expect_equal(
    rec_obj$steps[[1]]$na_percentage[[1]],
    34, tolerance = 3)

  expect_equal(
    names(rec_obj$steps[[1]]$na_percentage)[1],
    "character")

  expect_equal(
    rec_obj$steps[[1]]$na_percentage[[2]],
    10.8, tolerance = 3)

  expect_equal(
    names(rec_obj$steps[[1]]$na_percentage)[2],
    "suit")
})

test_that("Recipe is baked correctly on training data", {
  expect_true("missing" %in% levels(out_train$character))

  expect_equal(
    sum(out_train$character == "missing"),
    82, tolerance = 10)

  expect_true("missing" %in% levels(out_train$suit))

  expect_equal(
    sum(out_train$suit == "missing"),
    26, tolerance = 10)
})

test_that("Recipe is baked correctly on test data", {
  expect_true("missing" %in% levels(out_test$character))

  expect_equal(
    sum(out_test$character == "missing"),
    18, tolerance = 10)

  expect_true("missing" %in% levels(out_test$suit))

  expect_equal(
    sum(out_test$suit == "missing"),
    8, tolerance = 8)
})

test_that("Printer method works correctly within print.recipe()", {
  expect_output(
    print(rec_obj),
    regexp = "[Filling NA with missing for character, suit]"
  )
})

test_that("Warning is triggered for greater than 50% NA", {
  expect_warning(
   junk <- capture_output(
      junk <- prep(rec_obj2, training = d2_train)
   ),
  regexp = "[koopa: 61]"
  )
})

test_that("tidy method prints correctly", {
  res <- tidy(rec_obj$steps[[1]])
  expect_equal(res$terms, c("character", "suit", "is_goomba"))
  expect_equal(res$value, c(34, 10.8, 0), tolerance = 3)

  rec_obj <- recipe(is_goomba ~ ., data = d) %>% step_missing(all_nominal(), id = "id")
  expect_s3_class(tidy(rec_obj$steps[[1]]), "tbl_df")
})
