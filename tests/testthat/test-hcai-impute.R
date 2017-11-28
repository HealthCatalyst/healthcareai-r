context("Checking recipe step hcai-missing")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build dataset
n <- 300
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
d["is_goomba"] <- ifelse( (d["world"] - 2 * d["level"] - 1) > 0, "Y", "N")

# Add NAs
inds <- sample(1:n, 70, replace = FALSE)
d$level[inds] <- NA
inds <- sample(1:n, 50, replace = FALSE)
d$world[inds] <- NA
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

rec_obj <- recipe(is_goomba ~ ., data = d)

# Tests ------------------------------------------------------------------------
test_that("Bad rec_obj throws an error", {
  expect_error(hcai_impute(), 
               regexp = 'argument "rec_obj" is missing, with no default')
  expect_error(hcai_impute(rec_obj = "yeah hi!"),
               regexp = "rec_obj must be recipe object")
})

test_that("Defaults return mean on numeric, hcai on nominal", {
  rec_obj_new <- rec_obj %>%
    hcai_impute()

  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_meanimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_hcai_missing")
})

test_that("Non-supported methods throw errors.", {
  expect_error(
    rec_obj %>%
      hcai_impute(numeric_method = "guess"),
    regexp = "non-supported numeric method"
  )
  expect_error(
    rec_obj %>%
      hcai_impute(nominal_method = "guess"),
    regexp = "non-supported nominal method"
  )
})

test_that("bag impute called on both types", {
  rec_obj_new <- rec_obj %>%
    hcai_impute(numeric_method = "bagimpute")
  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_bagimpute")
  
  rec_obj_new <- rec_obj %>%
    hcai_impute(nominal_method = "bagimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_bagimpute")
})

test_that("knnimpute impute called on both types", {
  rec_obj_new <- rec_obj %>%
    hcai_impute(numeric_method = "knnimpute")
  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_knnimpute")
  
  rec_obj_new <- rec_obj %>%
    hcai_impute(nominal_method = "knnimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_knnimpute")
})

test_that("API takes knnimpute and bagimpute params", {
  rec_obj_new <- rec_obj %>%
    hcai_impute(numeric_method = "knnimpute",
                numeric_params = list(knn_K = 3))
  expect_equal(rec_obj_new$steps[[1]]$K, 3)
  
  rec_obj_new <- rec_obj %>%
    hcai_impute(nominal_method = "bagimpute",
                nominal_params = list(bag_options = list(nbagg = 10, 
                  keepX = FALSE)))
  expect_equal(rec_obj_new$steps[[2]]$options$nbagg, 10)
})

test_that("Default imputation methods bake expected results",{
  res <- capture_output(d_imputed <- rec_obj %>%
    hcai_impute() %>%
    prep(training = d_train) %>%
    bake(newdata = d_test))
  expect_equal(d_imputed$level[2], 6.31, tolerance = 2)
  expect_equal(d_imputed$character[2], "hcai_missing")
})

test_that("knn imputation bakes expected results",{
  res <- capture_output(d_imputed <- rec_obj %>%
                          hcai_impute(numeric_method = "knnimpute",
                            nominal_method = "knnimpute") %>%
                          prep(training = d_train) %>%
                          bake(newdata = d_test))
  expect_equal(d_imputed$level[2], 6.31, tolerance = 2)
  expect_equal(d_imputed$character[2], "hcai_missing")
})

test_that("bag imputation bakes expected results",{
  res <- capture_output(d_imputed <- rec_obj %>%
                          hcai_impute() %>%
                          prep(training = d_train) %>%
                          bake(newdata = d_test))
  expect_equal(d_imputed$level[2], 6.31, tolerance = 2)
  expect_equal(d_imputed$character[2], "hcai_missing")
})