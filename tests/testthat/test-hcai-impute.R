context("Checking hcai-impute recipe builder")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build hot dog set
n <- 300
df <- data.frame(id = 1:n,
                 vendorID = sample(1:9, size = n, replace = T),
                 length = rnorm(n, mean = 7, sd = 2),
                 diameter = rnorm(n, mean = 2, sd = 0.5),
                 heat = sample(c("Cold", "Hot"), size = n, replace = T),
                 condiment = sample(c("Ketchup", "Mustard", "Wasabi", "Syrup"),
                                    size = n, replace = T)
)

# give hotdog likeliness score
df["hot_dog"] <- df["length"] - 2 * df["diameter"] - 1
df$hot_dog[df["heat"] == "Hot"]  <-
  df$hot_dog[df["heat"] == "Hot"] + 1
df$hot_dog[df["heat"] == "Cold"]  <-
  df$hot_dog[df["heat"] == "Cold"] - 1
df$hot_dog[df["condiment"] == "Ketchup"] <-
  df$hot_dog[df["condiment"] == "Ketchup"] + 1
df$hot_dog[df["condiment"] == "Mustard"] <-
  df$hot_dog[df["condiment"] == "Mustard"] + 2
df$hot_dog[df["condiment"] == "Wasabi"] <-
  df$hot_dog[df["condiment"] == "Wasabi"] - 1
df$hot_dog[df["condiment"] == "Syrup"] <-
  df$hot_dog[df["condiment"] == "Syrup"] - 4


# Add noise
df$hot_dog <- df$hot_dog + rnorm(n, mean = 0, sd = 1.25)
df$hot_dog <- ifelse(df$hot_dog > 0, "Y", "N")

# Add missing data
df$condiment[sample(1:n, 32, replace = FALSE)] <- NA
df$length[sample(1:n, 51, replace = FALSE)] <- NA
df$heat[sample(1:n, 125, replace = FALSE)] <- NA
df$diameter[sample(1:n, 9, replace = FALSE)] <- NA

train_index <- caret::createDataPartition(
  df$hot_dog,
  p = 0.8,
  times = 1,
  list = TRUE)

d_train <- df[train_index$Resample1, ]
d_test <- df[-train_index$Resample1, ]

recipe <- recipe(hot_dog ~ ., data = d_train)

# Tests ------------------------------------------------------------------------
test_that("Bad recipe throws an error", {
  expect_error(hcai_impute(),
               regexp = "argument \"recipe\" is missing, with no default")
  expect_error(hcai_impute(recipe = "yeah hi!"),
               regexp = "recipe must be recipe object")
})

test_that("Defaults return mean on numeric, hcai on nominal", {
  rec_obj_new <- recipe %>%
    hcai_impute()

  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_meanimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_missing")
})

test_that("Non-supported methods throw errors.", {
  expect_error(
    recipe %>%
      hcai_impute(numeric_method = "guess"),
    regexp = "non-supported numeric method"
  )
  expect_error(
    recipe %>%
      hcai_impute(nominal_method = "guess"),
    regexp = "non-supported nominal method"
  )
})

test_that("Non-supported params throw warnings.", {
  expect_warning(
    recipe %>%
      hcai_impute(numeric_method = "knnimpute",
        numeric_params = list(knn_K = 5, more_cowbell = TRUE)),
    regexp = "more_cowbell"
  )
  expect_warning(
    recipe %>%
      hcai_impute(nominal_method = "bagimpute",
                  nominal_params = list(bag_model = "m", prune = TRUE)),
    regexp = "prune"
  )
})

test_that("bag impute called on both types", {
  rec_obj_new <- recipe %>%
    hcai_impute(numeric_method = "bagimpute")
  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_bagimpute")

  rec_obj_new <- recipe %>%
    hcai_impute(nominal_method = "bagimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_bagimpute")
})

test_that("knnimpute impute called on both types", {
  rec_obj_new <- recipe %>%
    hcai_impute(numeric_method = "knnimpute")
  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_knnimpute")

  rec_obj_new <- recipe %>%
    hcai_impute(nominal_method = "knnimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_knnimpute")
})

test_that("API takes knnimpute and bagimpute params", {
  rec_obj_new <- recipe %>%
    hcai_impute(numeric_method = "knnimpute",
                numeric_params = list(knn_K = 3))
  expect_equal(rec_obj_new$steps[[1]]$K, 3)

  rec_obj_new <- recipe %>%
    hcai_impute(nominal_method = "bagimpute",
                nominal_params = list(bag_options = list(nbagg = 10,
                  keepX = FALSE)))
  expect_equal(rec_obj_new$steps[[2]]$options$nbagg, 10)
})

test_that("Default imputation methods bake expected results", {
  res <- capture_output(d_imputed <- recipe %>%
    hcai_impute() %>%
    prep(training = d_train) %>%
    bake(newdata = d_test))
  expect_equal(d_imputed$length[13], 6.87, tolerance = 2)
  expect_equal(as.character(d_imputed$heat[3]), "missing")
})

test_that("knn imputation bakes expected results", {
  res <- capture_output(d_imputed <- recipe %>%
                          hcai_impute(numeric_method = "knnimpute",
                            nominal_method = "knnimpute") %>%
                          prep(training = d_train) %>%
                          bake(newdata = d_test))
  expect_equal(d_imputed$diameter[18], 2.16, tolerance = 2)
  expect_equal(as.character(d_imputed$condiment[3]), "Syrup")
})

test_that("bag imputation bakes expected results", {
  res <- capture_output(d_imputed <- recipe %>%
                          hcai_impute(numeric_method = "bagimpute",
                            nominal_method = "bagimpute",
                            numeric_params = list(seed_val = 30),
                            nominal_params = list(seed_val = 30)) %>%
                          prep(training = d_train) %>%
                          bake(newdata = d_test))
  expect_equal(as.character(d_imputed$heat[8]), "Cold")
  expect_equal(as.character(d_imputed$condiment[8]), "Mustard")
  expect_equal(d_imputed$length[14], 7.797, tolerance = 2)
})

test_that("random columns don't get imputed", {
  # If a categorical column is completely uncorrelated with other columns, bag
  # impute will not fill in the missing data. This test is mostly just to make
  # sure this function is behaving as it was when it was written.

  # add randomly distributed columns with NAs
  df$random_chars <- sample(c("NYC", "Chicago"), n, replace = TRUE)
  df$random_nums <- sample(1:n, n, replace = FALSE)
  df$random_chars[sample(1:n, 55, replace = FALSE)] <- NA
  df$random_nums[sample(1:n, 45, replace = FALSE)] <- NA

  d_train <- df[train_index$Resample1, ]
  d_test <- df[-train_index$Resample1, ]

  # trees
  recipe <- recipe(hot_dog ~ ., data = d_train)
  res <- capture_output(d_imputed <- recipe %>%
                          hcai_impute(numeric_method = "bagimpute",
                                      nominal_method = "bagimpute",
                                      numeric_params = list(seed_val = 30),
                                      nominal_params = list(seed_val = 30)) %>%
                          prep(training = d_train) %>%
                          bake(newdata = d_test))

  expect_true(any(missingness(d_imputed, return_df = FALSE) != 0))

  # knn
  recipe <- recipe(hot_dog ~ ., data = d_train)
  res <- capture_output(prepped <- recipe %>%
                          hcai_impute(numeric_method = "knnimpute",
                                      nominal_method = "knnimpute") %>%
                          prep(training = d_train))

  expect_error(prepped %>% bake(newdata = d_test))
})

test_that("all nominal or all numeric columns add 1 step", {
  num_dat <- dplyr::select_if(d_train, is.numeric)
  rec <-
    recipe( ~ ., num_dat) %>%
    hcai_impute() %>%
    prep(training = num_dat)
  expect_equal(length(rec$steps), 1)

  nom_dat <- dplyr::select_if(d_train, function(x) is.character(x) | is.factor(x))
  rec <-
    recipe( ~ ., nom_dat) %>%
    hcai_impute() %>%
    prep(training = nom_dat)
  expect_equal(length(rec$steps), 1)
})
