context("Testing prep_data")

# Setup ------------------------------------------------------------------------
# set seed for reproducibility
set.seed(7)
# build data set to predict whether or not animal_id is a is_ween
n <- 300
sample_days <- c("03-23-2008", "04-13-2008", "10-10-2008", "12-19-2008",
                 "05-27-2008", "07-20-2008", "09-22-2008", "01-13-2008")
df <- tibble(
  song_id = 1:n,
  length = rnorm(n, mean = 4, sd = 1),
  weirdness = rnorm(n, mean = 4, sd = 2),
  genre = sample(c("Rock", "Jazz", "Country"), size = n, replace = T),
  reaction = sample(c("Love", "Huh", "Dislike", "Mixed"),
                    size = n, replace = T, prob = c(4, 4, 4, 6)),
  guitar_flag = sample(c(0, 1), size = n, replace = T),
  drum_flag = sample(c(0, 1, NA), size = n, replace = T,
                     prob = c(0.45, 0.45, 0.1)),
  date_col = lubridate::ymd("2002-03-04") + lubridate::days(sample(1:1000, n)),
  posixct_col = seq(as.POSIXct("2005-1-1 0:00"), length.out = n, by = "hour"),
  col_DTS = lubridate::ymd("2006-03-01") + lubridate::days(sample(1:1000, n)),
  char_DTS = sample(sample_days, n, replace = TRUE),
  missing82 = sample(1:10, n, replace = TRUE),
  missing64 = sample(100:300, n, replace = TRUE),
  state = sample(c("NY", "MA", "CT", "CA", "VT", "NH"), size = n, replace = T,
                 prob = c(0.18, 0.1, 0.1, 0.6, 0.01, 0.01))
)
df$char_DTS <- as.character(df$char_DTS)

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
df$genre <- as.factor(df$genre)

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

d_prep <- prep_data(d = d_train, outcome = is_ween, song_id)
d_reprep <- prep_data(d_test, outcome = is_ween, song_id,
                      recipe = attr(d_prep, "recipe"))
d_reprep2 <- prep_data(d_test, outcome = is_ween, song_id,
                       recipe = d_prep)
animals <- tibble(
  animal = sample(c("cat", "dog", "mouse"), n, TRUE),
  weight = rexp(n, 0.5),
  super = sample(c(TRUE, FALSE), n, TRUE),
  y = rnorm(n)
)
animals$animal <- as.factor(animals$animal)
animals_train <- animals[1:250, ]
animals_test <- animals[251:300, ]

# Tests ------------------------------------------------------------------------
test_that("Bad data throws an error", {
  expect_error(prep_data(), regexp = "missing")
  expect_error(prep_data(d = "yeah hi!"), regexp = "data frame")
})

test_that("Bad outcome columns throws an error", {
  expect_error(prep_data(d = d_train, outcome = cowbell),
               regexp = "not found in d")
  d_train$is_ween[1:5] <- NA
  expect_error(prep_data(d = d_train, outcome = is_ween),
               regexp = "NA values")
})

test_that("Bad ignored columns throws an error", {
  expect_error(prep_data(d = d_train, cowbell),
               regexp = "not found in d")
  expect_error(prep_data(d = d_train, cowbell, spoons),
               regexp = "not found in d")
})

test_that("prep_data works with just param d", {
  d_clean <- prep_data(d = d_train, make_dummies = FALSE)
  expect_equal(unique(d_clean$weirdness[is.na(d_train$weirdness)]),
               mean(d_train$weirdness, na.rm = TRUE))
  expect_true(all.equal(droplevels(d_clean$genre[!is.na(d_train$genre)]),
                        d_train$genre[!is.na(d_train$genre)]))
  expect_true(all(d_clean$genre[is.na(d_train$genre)] == "missing"))
})

test_that("prep_data works with defaults and no ignore columns", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, make_dummies = FALSE)
  expect_equal(unique(d_clean$weirdness[is.na(d_train$weirdness)]),
               mean(d_train$weirdness, na.rm = TRUE))
  expect_true(all.equal(droplevels(d_clean$genre[!is.na(d_train$genre)]),
                        d_train$genre[!is.na(d_train$genre)]))
  expect_true(all(d_clean$genre[is.na(d_train$genre)] == "missing"))
})

test_that("prep_data works with defaults and one ignore column", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       make_dummies = FALSE)
  expect_equal(unique(d_clean$weirdness[is.na(d_train$weirdness)]),
               mean(d_train$weirdness, na.rm = TRUE))
  expect_true(all.equal(droplevels(d_clean$genre[!is.na(d_train$genre)]),
                        d_train$genre[!is.na(d_train$genre)]))
  expect_true(all(d_clean$genre[is.na(d_train$genre)] == "missing"))
})

test_that("prep_data works with defaults and two ignore columns", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id, state
                       , make_dummies = FALSE)
  expect_equal(unique(d_clean$weirdness[is.na(d_train$weirdness)]),
               mean(d_train$weirdness, na.rm = TRUE))
  expect_true(all.equal(droplevels(d_clean$genre[!is.na(d_train$genre)]),
                        d_train$genre[!is.na(d_train$genre)]))
  expect_true(all(d_clean$genre[is.na(d_train$genre)] == "missing"))
})

test_that("prep_data correctly attaches original data str", {
  ods <- attr(d_prep, "original_data_str")
  expect_s3_class(ods, "data.frame")
  expect_equal(0, nrow(ods))
  dtn <- names(d_train)
  expect_setequal(dtn[-which(dtn == "is_ween")], names(ods))
  expect_equal(ods, attr(d_reprep, "original_data_str"))
})

test_that("0/1 outcome is converted to N/Y", {
  d_train <- dplyr::mutate(d_train, is_ween = ifelse(is_ween == "Y", 1, 0))
  d_clean <- prep_data(d = d_train, outcome = is_ween)
  expect_s3_class(d_clean$is_ween, "factor")
  expect_true(all.equal(which(d_train$is_ween == 1),
                        which(d_clean$is_ween == "Y")))
  expect_true(all.equal(sort(levels(d_clean$is_ween)), c("N", "Y")))
})

test_that("date columns are found and converted", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       make_dummies = FALSE, convert_dates = "categories")
  expect_true(is.numeric(d_clean$posixct_col_hour))
  expect_true(is.factor(d_clean$date_col_dow))
  expect_true(is.factor(d_clean$posixct_col_month))
  expect_true(is.numeric(d_clean$col_DTS_year))
  expect_true(all(c("Jan", "Mar") %in% d_clean$date_col_month))
  expect_true(all(2006:2008 %in% d_clean$col_DTS_year))
  expect_true(all(c("Sun", "Mon", "Tue") %in% d_clean$col_DTS_dow))
  expect_true(all(c(0:23) %in% d_clean$posixct_col_hour))
})

test_that("date columns are found, converted, and dummified", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id, convert_dates = "categories")
  d_clean_dummied_months <- d_clean[grepl("date_col_month", names(d_clean))]
  # Should find 11 month names in names of prepped data as all but one get dummied
  expect_equal(sum(purrr::map_lgl(month.abb, ~ any(grepl(.x, names(d_clean_dummied_months))))), 11)
  expect_true("date_col_dow_Thu" %in% names(d_clean))
  expect_equal(sort(unique(d_clean$col_DTS_month_Sep)), c(0, 1))
  expect_true(all(2006:2008 %in% d_clean$col_DTS_year))
  expect_true(all(c(0:23) %in% d_clean$posixct_col_hour))
})

test_that("convert_dates works when continuous", {
  dd <- prep_data(d_train)
  expect_true("date_col_dow_sin" %in% names(dd))
  expect_false("date_col_dow" %in% names(dd))
  dd <- prep_data(d_train, convert_dates = "continuous")
  expect_true(all(c("date_col_dow_sin", "posixct_col_hour_cos") %in% names(dd)))
})

test_that("convert_dates removes date columns when none", {
  dd <- prep_data(d_train, convert_dates = "none")
  expect_true(!all(c("date_col", "posixct_col", "col_DTS") %in% names(dd)))
})

test_that("prep_data works when certain column types are missing", {
  d2 <- d_train %>%
    dplyr::select(-dplyr::one_of(c("date_col", "posixct_col",
                                   "col_DTS", "drum_flag", "guitar_flag")))
  d_clean <- prep_data(d = d2, outcome = is_ween, song_id, make_dummies = FALSE)
  expect_equal(unique(d_clean$weirdness[is.na(d2$weirdness)]),
               mean(d2$weirdness, na.rm = TRUE))
  expect_true(all.equal(droplevels(d_clean$genre[!is.na(d2$genre)]),
                        d2$genre[!is.na(d2$genre)]))
  expect_true(all(d_clean$genre[is.na(d2$genre)] == "missing"))
})

test_that("impute gives warning when column has 50% or more NA", {
  d_train$reaction[1:200] <- NA
  expect_warning(d_clean <- prep_data(d = d_train, outcome = is_ween, song_id),
                 regexp = "reaction")
})

test_that("impute fails with params and character columns", {
  suppressWarnings({
    expect_error(
      expect_message(
        d_clean <- prep_data(d_train, outcome = is_ween, song_id,
                             impute = list(numeric_method = "knnimpute",
                                           nominal_method = "bagimpute",
                                           numeric_params = list(knn_K = 5),
                                           nominal_params = NULL),
                             make_dummies = FALSE),
        regexp = "depends on another library"
      ), regexp = "class character"
    )
  })
})

test_that("impute works with params and factor columns", {
  d_train <- d_train %>% mutate(across(where(is.character), as.factor))
  suppressWarnings({
    d_clean <- prep_data(d_train, outcome = is_ween, song_id,
                         impute = list(numeric_method = "knnimpute",
                                       nominal_method = "bagimpute",
                                       numeric_params = list(knn_K = 5),
                                       nominal_params = NULL),
                         make_dummies = FALSE)
  })
  expect_true(is.numeric(d_clean$weirdness))
  expect_false(any(purrr::map_lgl(d_clean, ~any(is.na(.x)))))
  expect_true("missing" %in% levels(d_clean$genre))
})

test_that("impute works with partial/extra params", {
  d_train <- d_train %>% mutate(across(where(is.character), as.factor))
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       impute = list(numeric_method = "bagimpute"))
  m <- missingness(d_clean)
  expect_true(all(m$percent_missing == 0))
  expect_warning(
    d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                         impute = list(numeric_method = "knnimpute",
                                       numeric_params = list(knn_K = 5),
                                       the_best_params = "Moi!")),
    regexp = "the_best_params")

  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       impute = list(numeric_method = "locfimpute"))
  m <- missingness(d_clean)
  expect_true(all(m$percent_missing == 0))
})

test_that("rare factors go to other by default", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id)
  exp <- c("CA", "CT", "MA", "NY", "other")
  # Should get a column for all but one of the exp levels:
  expect_equal(sum(purrr::map_lgl(exp, ~ any(grepl(.x, names(d_clean))))),
               length(exp) - 1)
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id, make_dummies = FALSE)
  expect_true(all(exp %in% levels(d_clean$state)))
  exp <- c("Huh", "Love", "Mixed", "missing", "other")
  expect_true(all(exp %in% levels(d_clean$reaction)))
})

test_that("rare factors unchanged when FALSE", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       collapse_rare_factors = FALSE, make_dummies = FALSE,
                       add_levels = FALSE)
  exp <- c("CA", "CT", "MA", "NH", "NY", "VT", "missing")
  expect_equal(levels(d_clean$state), exp)
})

test_that("absent levels get dummified if rare factors are collapsed", {
  levels(d_train$genre) <- c(levels(d_train$genre), "Muzak")
  pd <- prep_data(d_train, song_id, collapse_rare_factors = FALSE)
  expect_true("genre_Muzak" %in% names(pd))
})

test_that("rare factors go to other when a threshold is specified", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       collapse_rare_factors = 0.15, make_dummies = FALSE,
                       add_levels = FALSE)
  exp <- c("CA", "NY", "other")
  expect_equal(levels(d_clean$state), exp)
})

test_that("centering and scaling work", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       center = TRUE, scale = TRUE)
  expect_equal(mean(d_clean$length), 0, tol = .01)
  expect_equal(mean(d_clean$weirdness), 0, tol = .01)
  expect_equal(sd(d_clean$length), 1, tol = .01)
  expect_equal(sd(d_clean$weirdness), 1, tol = .01)
})

test_that("dummy columns are created as expected", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       convert_dates = "none", make_dummies = TRUE,
                       add_levels = FALSE, collapse_rare_factors = FALSE)
  exp <- c("genre_Country", "genre_Jazz", "genre_Rock")
  n <- names(dplyr::select(d_clean, dplyr::starts_with("genre")))
  expect_true(all(exp %in% n))
  exp <- c("reaction_Dislike", "reaction_Huh", "reaction_Love",
           "reaction_missing")
  n <- names(dplyr::select(d_clean, dplyr::starts_with("reaction")))
  expect_true(all(n == exp))
})

test_that("test convert_dates is logical, `none`, `continuous`, or `categories`", {
  actual <- prep_data(d_train, convert_dates = TRUE)
  expect_true("date_col_dow_sin" %in% names(actual))

  actual <- prep_data(d_train, convert_dates = FALSE)
  expect_equal(sum(grepl("date_col_dow_sin", names(actual))), 0)

  expect_error(prep_data(d_train, convert_dates = "test"), "convert_dates must")
})

test_that("Output of impute is same for tibble vs data frame", {
  expect_equal(
    prep_data(d_train),
    prep_data(tibble::as_tibble(d_train)),
    check.attributes = FALSE)
})

test_that("recipe attr is a recipe class object", {
  dd <- prep_data(d_train)
  expect_true("recipe" %in% names(attributes(dd)))
  expect_s3_class(attr(dd, "recipe"), "recipe")
})

test_that("Training data are stored entirely in the recipe", {
  expect_equivalent(attr(d_prep, "recipe")$orig_data, d_train)
})

test_that("warning is given when ignored columns have missingness", {
  expect_warning(
    prep_data(d_train, reaction, length),
    regexp = "reaction and length")
})

test_that("names of ignored columns get attached as attribute to recipe", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id)
  expect_true("recipe" %in% names(attributes(d_clean)))
  recipe_attrs <- attributes(attr(d_clean, "recipe"))
  expect_true("ignored_columns" %in% names(recipe_attrs))
  expect_true(recipe_attrs$ignored_columns == "song_id")
  multi_ignore <- prep_data(d_train, song_id, guitar_flag, state)
  expect_true(all.equal(attr(attr(multi_ignore, "recipe"), "ignored_columns"),
                        c("song_id", "guitar_flag", "state")))
})

test_that("print works as expected", {
  messaged <- capture_messages(catted <- capture.output(d_prep))
  expect_true(any(grepl("Recipe", messaged)))
  expect_true(any(grepl("prepped", messaged)))
  expect_true(any(grepl("Recipe", catted)))
  expect_true(any(grepl("tibble", catted)))
})

test_that("prep_data applies recipe from training on test data", {
  expect_equal(d_reprep, d_reprep2)
  expect_equal(unique(d_reprep$weirdness[is.na(d_test$weirdness)]),
               mean(d_train$weirdness, na.rm = TRUE))
  # When missing is the reference level
  expect_equal(sum(d_reprep$genre_Country[is.na(d_test$genre)]), 0)
  expect_equal(sum(d_reprep$genre_Rock[is.na(d_test$genre)]), 0)
  expect_equal(sum(d_reprep$genre_Jazz[is.na(d_test$genre)]), 0)
})

test_that("Unignored variables present in training but not deployment error if needed", {
  expect_error(
    expect_warning(
      prep_data(dplyr::select(d_test, -length), recipe = d_prep)
    ),
    "length"
  )
  expect_s3_class(prep_data(dplyr::select(d_test, -song_id), recipe = d_prep),
                  "prepped_df")
})

test_that("New variables present in deployment get ignored with a warning", {
  big_d <- dplyr::mutate(d_train, extra = seq_len(nrow(d_train)))
  expect_warning(pd <- prep_data(big_d, recipe = attr(d_prep, "recipe")),
                 regexp = "extra")
  expect_true(all.equal(pd$extra, seq_len(nrow(d_train))))
})

test_that("All numeric variables aren't a problem", {
  d <- data.frame(x = 1:5, y = 5:1, id_var = letters[1:5])
  expect_s3_class(prep_data(d, id_var), "prepped_df")
  expect_s3_class(prep_data(d, outcome = "id_var"), "prepped_df")
})

test_that("All nominal variables aren't a problem", {
  d <- data.frame(x = rep(letters[1:5], 5), y = rep(letters[5:1], 5), id_var = 1:25)
  expect_s3_class(prep_data(d, id_var), "prepped_df")
  expect_s3_class(prep_data(d, outcome = id_var), "prepped_df")
})

test_that("remove_near_zero_variance is respected, works, and messages", {
  d_train <- dplyr::mutate(d_train,
                           a_nzv_col = c("rare", rep("common", nrow(d_train) - 1)))
  expect_message(def <- prep_data(d_train), regexp = "a_nzv_col")
  expect_false("a_nzv_col_rare" %in% names(def))
  # nzv_col should be removed in deployement even if it has variance
  d_test <- dplyr::mutate(d_test,
                          a_nzv_col = sample(letters, nrow(d_test), replace = TRUE))
  expect_message(pd <- prep_data(d_test, recipe = def), regexp = "a_nzv_col")
  expect_false("a_nzv_col_rare" %in% names(def))
  stay <- prep_data(d_train, remove_near_zero_variance = FALSE, make_dummies = FALSE)
  expect_true("a_nzv_col" %in% names(stay))
  expect_error(prep_data(dplyr::select(d_train, a_nzv_col, is_ween), outcome = is_ween),
               "less aggressive")
  # Check that NZV columns missing in deployment warn but don't error
  expect_warning(prep_data(dplyr::select(d_test, -a_nzv_col), recipe = def),
                 "present in training")
})

test_that("remove_near_zero_variance works with params", {
  # Meets criteria
  d_train <- dplyr::mutate(d_train,
                           a_nzv_col = c("rare", rep("common", nrow(d_train) - 1)))
  expect_error(def <- prep_data(d_train,
                                remove_near_zero_variance = "chowder"),
               regexp = "logical or numeric")
  expect_message(def <- prep_data(d_train,
                                  remove_near_zero_variance = 0.01)
                 , regexp = "a_nzv_col")
  expect_false("a_nzv_col_rare" %in% names(def))
  # nzv_col should be removed in deployement even if it has variance
  d_test <- dplyr::mutate(d_test,
                          a_nzv_col = sample(letters, nrow(d_test), replace = TRUE))
  expect_message(pd <- prep_data(d_test, recipe = def), regexp = "a_nzv_col")
  expect_false("a_nzv_col_rare" %in% names(def))

  expect_error(def <- d_train %>%
                 select(guitar_flag, a_nzv_col, length) %>%
                 prep_data(remove_near_zero_variance = .1),
               regexp = "less aggressive")

  # Doesn't meet criteria
  d_train$a_nzv_col[1:30] <- "rare"
  d_train$a_nzv_col[31:50] <- letters[1:20]
  def <- prep_data(d_train,
                   remove_near_zero_variance = .11)
  expect_true("a_nzv_col_rare" %in% names(def))
  # nzv_col should be removed in deployement even if it has variance
  d_test <- dplyr::mutate(d_test,
                          a_nzv_col = sample(letters, nrow(d_test), replace = TRUE))
  pd <- prep_data(d_test, recipe = def)
  expect_true("a_nzv_col_rare" %in% names(def))
})

test_that("collapse_rare_factors works", {
  d_train <- dplyr::mutate(d_train, imbal = c(letters, rep("common", nrow(d_train) - 26)))
  pd <- prep_data(d_train,
                  collapse_rare_factors = FALSE,
                  make_dummies = FALSE)
  expect_true(all(letters %in% pd$imbal))
  coll <- prep_data(d_train, make_dummies = FALSE)
  expect_false(any(letters %in% coll$imbal))
})

test_that("collapse_rare_factors respects threshold", {
  # 1% < a < 2%
  d_train <- dplyr::mutate(d_train, imbal = c(rep("a", 4),
                                              rep("common", nrow(d_train) - 4)))
  def <- prep_data(d_train, remove_near_zero_variance = FALSE,
                   make_dummies = FALSE)
  below_thresh <- prep_data(d_train, remove_near_zero_variance = FALSE,
                            make_dummies = FALSE, collapse_rare_factors = .02)
  above_thresh <- prep_data(d_train, remove_near_zero_variance = FALSE,
                            make_dummies = FALSE, collapse_rare_factors = .01)
  never <- prep_data(d_train, remove_near_zero_variance = FALSE,
                     make_dummies = FALSE, collapse_rare_factors = FALSE)
  expect_false("a" %in% def$imbal)
  expect_false("a" %in% below_thresh$imbal)
  expect_true("a" %in% above_thresh$imbal)
  expect_true("a" %in% never$imbal)
  expect_true("other" %in% def$imbal)
  expect_true("other" %in% below_thresh$imbal)
  expect_false("other" %in% above_thresh$imbal)
  expect_false("other" %in% never$imbal)
})

test_that("missing and other are added to all nominal columns", {
  d <- data.frame(has_missing = c(rep(NA, 10), rep("b", 20)),
                  has_rare = c("rare", rep("common", 29)),
                  has_both = c("rare", NA, rep("common", 28)),
                  has_neither = c(rep("cat1", 15), rep("cat2", 15)))
  pd <- prep_data(d, make_dummies = FALSE, remove_near_zero_variance = FALSE,
                  collapse_rare_factors = .05)
  expect_true(all(purrr::map_lgl(pd, ~ all(c("other", "missing") %in% levels(.x)))))

  scrambled <- data.frame(has_missing = c(rep("cat1", 15), rep("cat2", 15)),
                          has_rare = c(rep(NA, 10), rep("b", 20)),
                          has_both = c("rare", rep("common", 29)),
                          has_neither = c("rare", NA, rep("common", 28)))
  suppressWarnings(scrambled_prep <- prep_data(scrambled, recipe = attr(pd, "recipe")))
  expect_true(all(purrr::map_lgl(scrambled_prep, ~ all(c("other", "missing") %in% levels(.x)))))
})

test_that("add_levels doesn't add levels to outcome", {
  d <- data.frame(x = c("A", "B"), y = 1:2)
  pd <- prep_data(d, outcome = x, make_dummies = FALSE, add_levels = FALSE)
  expect_false(any(c("other", "missing") %in% levels(pd$x)))
})

test_that("prep_data respects add_levels = FALSE", {
  d <- data.frame(x = c("A", "B", "B"), y = 1:3)
  pd <- prep_data(d, add_levels = FALSE, make_dummies = FALSE,
                  impute = FALSE, collapse_rare_factors = FALSE)
  expect_false(any(c("other", "missing") %in% levels(pd$x)))
})

test_that("If outcome to prep_data is or looks like logical, get informative error", {
  d_train <- d_train %>% dplyr::mutate(is_ween = is_ween == "Y")
  expect_error(prep_data(d_train, outcome = is_ween), "logical")
  d_train <- d_train %>% dplyr::mutate(is_ween = as.character(is_ween == "Y"))
  expect_error(prep_data(d_train, outcome = is_ween), "logical")
})

test_that("prep_data leaves newly created dummies as 0/1", {
  d <- data.frame(x = 1:10, y = rep(letters[1:2], len = 10))
  expect_true(all(prep_data(d, center = TRUE, scale = TRUE)[["y_b"]] %in% 0:1))
})

test_that("If recipe provided but no outcome column, NA-outcome column isn't created", {
  expect_false("is_ween" %in% names(
    prep_data(dplyr::select(d_test, -is_ween), song_id, recipe = attr(d_prep, "recipe"))
  ))
})

test_that("prep_data doesn't remove outcome variable specified in recipe", {
  d <- data.frame(x = 1:5, y = 6:10)
  pd <- prep_data(d, outcome = y)
  expect_true("y" %in% names(prep_data(data.frame(x = 1, y = 2), recipe = pd)))
})

test_that("predict regression with prep doesn't choke without outcome", {
  prep_data(pima_diabetes[1:50, ], outcome = age) %>%
    tune_models(age, models = "rf") %>%
    predict(dplyr::select(pima_diabetes[51:55, ], -age)) %>%
    expect_s3_class("predicted_df")
})

test_that("predict classification with prep doesn't choke without outcome", {
  prep_data(pima_diabetes[1:50, ], outcome = diabetes) %>%
    tune_models(diabetes, models = "rf") %>%
    predict(dplyr::select(pima_diabetes[51:55, ], -diabetes)) %>%
    expect_s3_class("predicted_df")
})

test_that("prep_data attaches missingness to recipe as attribute", {
  expect_true("missingness" %in% names(attributes(attr(d_prep, "recipe"))))
  expect_equal(attr(attr(d_prep, "recipe"), "missingness"),
               missingness(d_train, return_df = FALSE))
})

test_that("prep_data warns when there's missingness in a column that didn't have missingness in training", {
  d_test$guitar_flag[1] <- NA
  expect_warning(prep_data(d_test, recipe = d_prep), "guitar_flag")
})

test_that("prep_data doesn't warn for new missingness in ID or outcome columns", {
  d_test$is_ween[1:5] <- NA
  d_test$song_id[1:5] <- NA
  expect_warning(prep_data(d_test, recipe = d_prep), NA)
})

test_that("prep_data attaches factor levels to recipe as attribute", {
  rec <- attr(d_prep, "recipe")
  expect_true("factor_levels" %in% names(attributes(rec)))
  expect_setequal(names(attr(rec, "factor_levels")), c("genre", "reaction", "state", "is_ween"))
})

test_that("prep_data tells you if you forgot to name your outcome argument", {
  expect_message(prep_data(d_train, song_id, is_ween), "outcome")
})

test_that("prep_data warns iff factor contrast options are dummy", {
  oc <- options("contrasts")
  on.exit(options(contrasts = oc[[1]]))
  options(contrasts = c("contr.dummy", "contr.poly"))
  expect_warning(prep_data(d_train, song_id, outcome = is_ween), "contrasts")
  options(contrasts = c("contr.treatment", "contr.poly"))
  expect_warning(prep_data(d_train, song_id, outcome = is_ween), NA)
})

test_that("prep_data errors informatively 0/1 factor outcomes", {
  dd <- data.frame(y = factor(rep(0:1, 10)),
                   x1 = sample(letters[1:10], 20, replace = TRUE),
                   x2 = rnorm(20))
  expect_error(prep_data(dd, outcome = y), "character")
})

test_that("data prepped on existing recipe returns ID columns", {
  # Only difference in the prep here is the ID col isn't specified,
  # but that's remembered in the recipe.
  setdiff(names(d_reprep),
          names(prep_data(d_test, recipe = attr(d_prep, "recipe"))))
})

test_that("prep_data outcome or ignored columns can be provided quoted", {
  std <- prep_data(d_train, song_id, state, outcome = is_ween)
  expect_equal(prep_data(d_train, song_id, state, outcome = "is_ween"), std,
               check.attributes = FALSE)
  expect_equal(prep_data(d_train, song_id, "state", outcome = is_ween), std,
               check.attributes = FALSE)
  expect_equal(prep_data(d_train, "song_id", "state", outcome = "is_ween"), std,
               check.attributes = FALSE)
})

test_that("prep_data warns for all unique character columns and adds the to ignored", {
  d_test$id <- paste0("a", seq_len(nrow(d_test)))
  expect_warning(pp <- prep_data(d_test, song_id), "id")
  expect_setequal(attributes(attr(pp, "recipe"))$ignored_columns, c("song_id", "id"))
})

test_that("prep_data doesn't check for all-unique columns in predict", {
  d_train <-
    d_train %>%
    dplyr::mutate_if(is.factor, as.character)

  # Additional test of this in the last code chunk of healthcareai.Rmd
  d_prep <- prep_data(d = d_train, outcome = is_ween, song_id,
                      convert_dates = "none", make_dummies = FALSE)
  d_test$state <- c("CA", paste0("A", seq_len(nrow(d_test) - 1)))
  expect_true("state" %in% names(pd <- prep_data(d_test, recipe = d_prep)))
  expect_equal("CA", as.character(pd$state[1]))
})

test_that("prep_data gets rid of logicals", {
  prepped_train <- prep_data(animals_train, outcome = y)
  prepped_test <- prep_data(animals_test, recipe = prepped_train, outcome = y)
  expect_false(any(purrr::map_lgl(prepped_train, is.logical)))
  expect_false(any(purrr::map_lgl(prepped_test, is.logical)))
})

test_that("prep_data gets rid of logicals, when no outcome", {
  prepped_train <- prep_data(animals_train)
  prepped_test <- prep_data(animals_test, recipe = prepped_train)
  expect_false(any(purrr::map_lgl(prepped_train, is.logical)))
  expect_false(any(purrr::map_lgl(prepped_test, is.logical)))
})

test_that("test ref levels input - and step_dummy_hcai attributes", {
  d_clean <- prep_data(d = d_train, outcome = is_ween, song_id,
                       make_dummies = list(genre = "Jazz", reaction = "Dislike"))
  expect_false("genre_Jazz" %in% names(d_clean))
  expect_false("reaction_Dislike" %in% names(d_clean))

  # Ref levels and dummies are stored in step_dummy_hcai
  steps <- (d_clean %>% attr("recipe"))$step
  loc <- purrr::map_lgl(steps, ~{
    class(.x) %>% first() == "step_dummy_hcai"
  }
  )
  dummies <- steps[loc][[1]]$dummies
  ref_levels <- steps[loc][[1]]$ref_levels

  expect_true("genre_Country" %in% dummies$dummy)
  expect_true("reaction_Love" %in% dummies$dummy)
  expect_true("Jazz" %in% dummies$ref)
  expect_true("Dislike" %in% dummies$ref)

  expect_equal(ref_levels, c(genre = "Jazz", reaction = "Dislike",
                             state = "CA"))
})

test_that("no_prep dominates", {
  noprep <- prep_data(animals_train, outcome = y, no_prep = TRUE)
  expect_true(attr(attr(noprep, "recipe"), "no_prep"))
  expect_equivalent(animals_train, noprep)
  expect_equivalent(animals_test, prep_data(animals_test, recipe = noprep))
})

test_that("prep_data doesn't throw warning if DTS character column is unique", {
  d_train <-
    animals_train %>%
    mutate(
      admitted_DTS = seq(as.POSIXct("2005-1-1 0:00"),
                         length.out = nrow(animals_train), by = "hour")
    )
  d_train$admitted_DTS <- as.character(d_train$admitted_DTS)
  prepped <- prep_data(d_train)
  admitted_dTS_cols <- map_lgl(names(prepped), ~{
    grepl("admitted_DTS", .x)
  }
  )
  expect_true(sum(admitted_dTS_cols) > 1)
})

test_that("make_dummies throws error if not logical or list", {
  expect_error(
    prep_data(d_train, make_dummies = "Test"),
    "step_dummies must be logical or list"
  )
})
