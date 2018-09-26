context("Testing PCA functionality in prep_data")
n <- 300
sample_days <- c("03-23-2008", "04-13-2008", "10-10-2006", "12-19-2007",
                 "05-27-2008", "07-20-2009", "09-22-2010", "01-13-2011")
df <- data.frame(
  song_id = 1:n,
  length = rnorm(n, mean = 4, sd = 1),
  weirdness = rnorm(n, mean = 4, sd = 2),
  genre = sample(c("Rock", "Jazz", "Country"), size = n, replace = T),
  reaction = sample(c("Love", "Huh", "Dislike", "Mixed"),
                    size = n, replace = T),
  guitar_flag = sample(c(0, 1), size = n, replace = T),
  drum_flag = sample(c(0, 1, NA), size = n, replace = T,
                     prob = c(0.45, 0.45, 0.1)),
  date_col = lubridate::ymd("2002-03-04") + lubridate::days(sample(1:1000, n)),
  posixct_col = lubridate::ymd("2004-03-04") + lubridate::days(sample(1:1000, n)),
  col_DTS = lubridate::ymd("2006-03-01") + lubridate::days(sample(1:1000, n)),
  char_DTS = sample(sample_days, n, replace = TRUE),
  missing82 = sample(1:10, n, replace = TRUE),
  missing64 = sample(100:300, n, replace = TRUE),
  state = sample(c("NY", "MA", "CT", "CA", "VT", "NH"), size = n, replace = T,
                 prob = c(0.18, 0.1, 0.1, 0.6, 0.01, 0.01))
)
df$char_DTS <- as.character(df$char_DTS)
df$genre <- as.character(df$genre)
df$reaction <- as.character(df$reaction)
df$state <- as.character(df$state)
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
d_train$weirdness[3] <-  d_test$weirdness[3] <- NA
d_train$genre[3] <- d_test$genre[3] <- NA


#TEST - PCA param must be logical or integer
test_that("PCA must be logical or numeric", {
  expect_error(prep_data(d = d_train, outcome = is_ween, PCA = "nope"),
               regexp = "PCA must be logical or numeric")
})

#TEST - Make sure error with missing data
test_that("Can't run with missing data", {
  expect_error(suppressWarnings(prep_data(d = d_train, outcome = is_ween, impute = FALSE, PCA = 5)),
               regexp = "NAs present in \"d\". PCA not compatible when NAs are present.")
})

#TEST - make sure no error when center/scale is false
test_that("PCA overwrites center and scale", {
  expect_warning(prep_data(d = d_train, outcome = is_ween, PCA = 3, center = FALSE),
                 regexp = "\"d\" must be centered and scaled to perform PCA. Center and Scale are being set to TRUE.")
  expect_s3_class(suppressWarnings(prep_data(d = d_train, outcome = is_ween, PCA = TRUE, center = FALSE)), "prepped_df")

})

#TEST - Make sure number of PC columns matches input param
test_that("Number of PC columns matches input param", {
  nPCs <- 7
  d_PCA <- suppressWarnings(prep_data(d = d_train, outcome = is_ween, PCA = nPCs))
  res <- d_PCA %>%
    select(starts_with("PC")) %>%
    ncol()
  expect_equal(res, nPCs)
})

#PCA ignores ignored columns and outcome variables
test_that("PCA ignores outcome and ignored cols", {
  d_PCAignored <- suppressWarnings(prep_data(d = d_train, song_id, outcome = is_ween, PCA = 5))
  d_train2 <- d_train %>% select(-song_id, -is_ween)
  expect_equal(ncol(d_train2[, names(d_train2) %in% names(d_PCAignored)]), 0)
  expect_equal(ncol(d_PCAignored[, names(d_PCAignored) %in% c("song_id", "is_ween")]), 2)
})

#TEST - Make sure errors if too high param
test_that("Errors if number of PCs is higher than width of prepped dataframe", {
  dfwidth <- ncol(prep_data(d = d_train, outcome = is_ween))
  expect_error(suppressWarnings(prep_data(d = d_train, outcome = is_ween, PCA = (dfwidth + 1))))
})
#TEST - PCA works with all numeric columns or all character columns
test_that("PCA works with all numeric columns or all character columns", {
  d_nums <- select_if(d_train, is.numeric)
  d_chars <- select_if(d_train, is.character) %>% select(-char_DTS)
  expect_s3_class(suppressWarnings(prep_data(d = d_nums, song_id, outcome = is_ween, PCA = 5)), "prepped_df")
  expect_s3_class(suppressWarnings(prep_data(d = d_chars, outcome = state, PCA = 2)), "prepped_df")
})
#TEST - Make sure flash_models, tune_models, and predict work for pca'd data
test_that("flash_models, tune_models, and predict work", {
  d_prep <- suppressWarnings(prep_data(d = d_train, song_id, outcome = is_ween, PCA = 5))
  expect_error(pca_mod <- tune_models(d_prep, outcome = is_ween, tune_depth = 1), NA)
  expect_error(pca_flash <- flash_models(d_prep, outcome = is_ween), NA)
  expect_error(pca_oldpreds <- predict(pca_mod), NA)
  expect_error(pca_preds <- predict(pca_mod, newdata = d_test), NA)
  expect_s3_class(pca_oldpreds, "predicted_df")
  expect_s3_class(pca_preds, "predicted_df")
})

#TEST - Make sure interpret and get_variable_importance return PCA columns, not originals

 test_that("Interpret and get_variable_importance return PCA columns", {
   d_prep <- suppressWarnings(prep_data(d = d_train, song_id, outcome = is_ween, PCA = 5))
   pca_glm <- tune_models(d_prep, outcome = is_ween, tune_depth = 1, models = "glm")
   expect_equal(sum(!grepl("PC", interpret(pca_glm)$variable)), 1)
   pca_rf <- tune_models(d_prep, outcome = is_ween, tune_depth = 1, models = "rf")
   expect_equal(sum(!grepl("PC", get_variable_importance(pca_rf)$variable)), 0)
 })
