context("Checking hcai-impute recipe builder")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build hot dog set
n <- 300
df <- tibble(id = 1:n,
             vendorID = sample(1:9, size = n, replace = T),
             length = rnorm(n, mean = 7, sd = 2),
             diameter = rnorm(n, mean = 2, sd = 0.5),
             heat = sample(c("Cold", "Hot"), size = n, replace = T),
             condiment = sample(c("Ketchup", "Mustard", "Wasabi", "Syrup"),
                                size = n, replace = T)
)
conds <- as.character(unique(df$condiment))
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
df$hot_dog <- as.factor(df$hot_dog)

# Add missing data
df$condiment[sample(1:n, 32, replace = FALSE)] <- NA
df$length[sample(1:n, 51, replace = FALSE)] <- NA
df$heat[sample(1:n, 125, replace = FALSE)] <- NA
df$diameter[sample(1:n, 9, replace = FALSE)] <- NA

df <- df %>% mutate(across(.cols = 5:6, .fns = as.factor))

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

  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_impute_mean")
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
                  numeric_params = list(knn_K = 5, bag_model = "m")),
    regexp = "bag_model"
  )
  expect_warning(
    recipe %>%
      hcai_impute(nominal_method = "bagimpute",
                  nominal_params = list(bag_model = "m", knn_K = 5)),
    regexp = "knn_K"
  )
  expect_warning(
    recipe %>%
      hcai_impute(nominal_method = "new_category",
                  nominal_params = list(knn_K = 5)),
    regexp = "knn_K"
  )
  expect_warning(
    recipe %>%
      hcai_impute(numeric_method = "mean",
                  numeric_params = list(bag_model = "m")),
    regexp = "bag_model"
  )
  expect_warning(
    recipe %>%
      hcai_impute(numeric_method = "locfimpute",
                  numeric_params = list(bag_model = "m")),
    regexp = "bag_model"
  )
})

test_that("bag impute called on both types", {
  rec_obj_new <- recipe %>%
    hcai_impute(numeric_method = "bagimpute")
  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_impute_bag")

  rec_obj_new <- recipe %>%
    hcai_impute(nominal_method = "bagimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_impute_bag")
})

test_that("knnimpute impute called on both types", {
  rec_obj_new <- recipe %>%
    hcai_impute(numeric_method = "knnimpute")
  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_impute_knn")

  rec_obj_new <- recipe %>%
    hcai_impute(nominal_method = "knnimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_impute_knn")
})

test_that("knnimpute impute called on both types", {
  rec_obj_new <- recipe %>%
    hcai_impute(numeric_method = "locfimpute")
  expect_equal(class(rec_obj_new$steps[[1]])[1], "step_locfimpute")

  rec_obj_new <- recipe %>%
    hcai_impute(nominal_method = "locfimpute")
  expect_equal(class(rec_obj_new$steps[[2]])[1], "step_locfimpute")
})

test_that("API takes knnimpute and bagimpute params", {
  rec_obj_new <- recipe %>%
    hcai_impute(numeric_method = "knnimpute",
                numeric_params = list(knn_K = 3))
  expect_equal(rec_obj_new$steps[[1]]$neighbors, 3)

  rec_obj_new <- recipe %>%
    hcai_impute(nominal_method = "bagimpute",
                nominal_params = list(bag_trees = 10))
  expect_equal(rec_obj_new$steps[[2]]$trees, 10)
})

test_that("Default imputation methods bake expected results", {
  res <- capture_output(d_imputed <- recipe %>%
    hcai_impute() %>%
    prep(training = d_train) %>%
    bake(new_data = d_test))
  expect_equal(d_imputed$length[13], 6, tolerance = 6)
  expect_equal(as.character(d_imputed$heat[3]), "missing")
})

test_that("knn imputation bakes expected results", {
  res <- capture_output(d_imputed <- recipe %>%
                          hcai_impute(numeric_method = "knnimpute",
                            nominal_method = "knnimpute") %>%
                          prep(training = d_train) %>%
                          bake(new_data = d_test))
  expect_equal(d_imputed$diameter[18], 2, tolerance = 2)
  expect_true(as.character(d_imputed$condiment[3]) %in% conds)
})

test_that("bag imputation bakes expected results", {
  res <- capture_output(d_imputed <- recipe %>%
                          hcai_impute(numeric_method = "bagimpute",
                            nominal_method = "bagimpute",
                            numeric_params = list(seed_val = 30),
                            nominal_params = list(seed_val = 30)) %>%
                          prep(training = d_train) %>%
                          bake(new_data = d_test))
  expect_true(as.character(d_imputed$heat[8]) %in% c("Hot", "Cold"))
  expect_true(as.character(d_imputed$condiment[8]) %in% conds)
  expect_equal(d_imputed$length[14], 6, tolerance = 6)
})

test_that("locf imputation bakes expected results", {
  d_imputed <- recipe %>%
    hcai_impute(numeric_method = "locfimpute",
                nominal_method = "locfimpute") %>%
    prep(training = d_train) %>%
    bake(new_data = d_test)
  expect_true(as.character(d_imputed$heat[8]) %in% c("Hot", "Cold"))
  expect_true(as.character(d_imputed$condiment[8]) %in% conds)
  expect_equal(d_imputed$length[14], 6, tolerance = 6)
})

test_that("random columns get imputed when factors", {
  # add randomly distributed columns with NAs
  df$random_chars <- sample(c("NYC", "Chicago"), n, replace = TRUE)
  df$random_nums <- sample(1:n, n, replace = FALSE)
  df$random_chars[sample(1:n, 55, replace = FALSE)] <- NA
  df$random_nums[sample(1:n, 45, replace = FALSE)] <- NA
  df$random_chars <- as.factor(df$random_chars)

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
                          bake(new_data = d_test))

  expect_false(any(missingness(d_imputed, return_df = FALSE) != 0))

  # knn
  recipe <- recipe(hot_dog ~ ., data = d_train)
  res <- capture_output(prepped <- recipe %>%
                          hcai_impute(numeric_method = "knnimpute",
                                      nominal_method = "knnimpute") %>%
                          prep(training = d_train))

  expect_error(prepped %>% bake(new_data = d_test), NA)
})

test_that("all nominal or all numeric columns add 1 step", {
  num_dat <- dplyr::select_if(d_train, is.numeric)
  rec <-
    recipe(~ ., num_dat) %>%
    hcai_impute() %>%
    prep(training = num_dat)
  expect_equal(length(rec$steps), 1)

  nom_dat <- dplyr::select_if(d_train, function(x) is.character(x) | is.factor(x))
  rec <-
    recipe(~ ., nom_dat) %>%
    hcai_impute() %>%
    prep(training = nom_dat)
  expect_equal(length(rec$steps), 1)
})

test_that("test warning for bag imputation mal function", {
  df$heat <- as.character(df$heat)
  df$condiment <- as.character(df$condiment)

  out_data <-
    recipe(hot_dog ~ ., data = df) %>%
    hcai_impute(nominal_method = "bagimpute") %>%
    prep() %>%
    bake(new_data = df)

  expect_true(!any(is.na(out_data)))

  expect_message(
    my_recipe <-
      recipe(hot_dog ~ ., data = df) %>%
      hcai_impute(numeric_method = "knnimpute"),
    "`knnimpute` depends on another library"
  )

  expect_error(
    prep(my_recipe) %>%
      bake(new_data = df),
    regexp = "factor"
  )

  my_recipe <-
    recipe(hot_dog ~ ., data = df) %>%
    hcai_impute(nominal_method = "knnimpute")

  expect_error(
    prep(my_recipe) %>%
      bake(new_data = df),
    regexp = "factor"
  )
})
