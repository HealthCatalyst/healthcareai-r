context("Checking pivot")

# Setup ------------------------------------------------------------------------
dd <- tibble::tibble(person = as.factor(rep(c("Jack", "Jane"), each = 6)),
                     day = as.factor(rep(1:2, 6)),
                     count = 1:12,
                     activity = rep(c("shower", "shower", "bath"), each = 4))

# Test pivot -------------------------------------------------------------------
test_that("pivot fails informatively if any provided columns aren't present", {
  expect_error(pivot(dd, person, group), regexp = "group")
  expect_error(pivot(dd, whatev, day, count), regexp = "whatev")
  expect_error(pivot(dd, person, day, not_here), regexp = "not_here")
  expect_error(pivot(dd, aaa, bbb, ccc), regexp = "aaa")
  expect_error(pivot(dd, aaa, bbb, ccc), regexp = "ccc")
})

test_that("pivot prints message if no fill column or no function provided", {
  expect_message(pivot(dd, person, day), regexp = "function")
  expect_message(pivot(dd, person, day, count), regexp = "function")
  expect_message(pivot(dd, person, day, fun = length), regexp = "fill")
})


test_that("pivot fills in NAs where no instances present, and only there", {
  suppressWarnings(
    mark_missing <-
      tibble::tibble(person = "Mark",
                     day = 1,
                     count = 13,
                     activity = "bath") %>%
      rbind(dd, .) %>%
      pivot(person, day, count)
  )
  expect_true(is.na(mark_missing$day_2[mark_missing$person == "Mark"]))
  expect_false(is.na(mark_missing$day_1[mark_missing$person == "Mark"]))
  expect_false(is.na(mark_missing$day_2[mark_missing$person == "Jane"]))
})

test_that("pivot returns expected data frame summing column", {
    expected <- tibble::tibble(
      person = factor(c("Jack", "Jane")),
      day_1 = c(9L, 27L),
      day_2 = c(12L, 30L)
    )
    actual <- pivot(dd, person, day, count, sum)
  expect_equal(expected, actual, check.attributes = FALSE)
})

test_that("pivot returns expected data frame with no fill column", {
  expect_equal(
    tibble::tibble(
      person = factor(c("Jack", "Jane")),
      day_1 = c(3L, 3L),
      day_2 = c(3L, 3L)
    ),
    suppressWarnings(pivot(dd, person, day)),
    check.attributes = FALSE
  )
})

test_that("pivot returns expected data frame with custom function", {
  expect_equal(
    tibble::tibble(
      person = factor(c("Jack", "Jane")),
      day_1 = c(6L, 4L),
      day_2 = c(6L, 4L)
    ),
    pivot(dd, person, day, fill = activity, fun = function(x) min(nchar(x))),
    check.attributes = FALSE
  )
})

test_that("pivot doesn't change grain class", {
  dd$person <- as.character(dd$person)
  dd$day <- as.character(dd$day)
  pivoted <- pivot(dd, person, day, count)
  expect_equal(class(dd$person), class(pivoted$person))
})

test_that("extra_cols works when supplied", {
  ec <- pivot(dd, person, day, count, extra_cols = c("hey", "you"))
  expect_true(all(c("day_hey", "day_you") %in% names(ec)))
  expect_true(all(is.na(ec$day_you)))
  expect_equal(class(ec$day_1), class(ec$day_hey))

  ec0 <- pivot(dd, person, day, count, missing_fill = 0, extra_cols = c("hey", "you"))
  expect_true(all(c("day_hey", "day_you") %in% names(ec0)))
  expect_true(all(ec0$day_you == 0))
  expect_equal(class(ec0$day_1), class(ec0$day_hey))
})

# Test do_aggregate ------------------------------------------------------------
test_that("do_aggregate produces messages appropriately", {
  expect_message(
    do_aggregate(d = dd,
                 grain = rlang::quo(person),
                 spread = rlang::quo(day),
                 fill = rlang::quo(count),
                 fun = sum,
                 default_fun = TRUE),
    regexp = "didn't provide a function"
  )
  expect_message(
    do_aggregate(d = dd,
                 grain = rlang::quo(person),
                 spread = rlang::quo(day),
                 fill = rlang::quo(count),
                 fun = sum,
                 default_fun = FALSE),
    regexp = NA
  )
})

test_that("do_aggregate produces informative error if aggregation failed", {
  expect_error(
    do_aggregate(d = dd,
                 grain = rlang::quo(person),
                 spread = rlang::quo(day),
                 fill = rlang::quo(count),
                 fun = function(x) x - 1,
                 default_fun = FALSE),
    regexp = "No aggregration occured"
  )
})

# Test aggregate_rows ----------------------------------------------------------
agg_dd <- aggregate_rows(d = dd,
                         grain = rlang::quo(person),
                         spread = rlang::quo(day),
                         fill = rlang::quo(count),
                         fun = sum)

test_that("aggregate_rows returns an ungrouped data frame shorter than input", {
  expect_true(inherits(agg_dd, "data.frame"))
  expect_false(dplyr::is.grouped_df(agg_dd))
  expect_true(nrow(agg_dd) < nrow(dd))
})

test_that("aggregate_rows sums correctly by default", {
  expect_equal(agg_dd$count,
               c(sum(dd$count[dd$person == "Jack" & dd$day == "1"]),
                 sum(dd$count[dd$person == "Jack" & dd$day == "2"]),
                 sum(dd$count[dd$person == "Jane" & dd$day == "1"]),
                 sum(dd$count[dd$person == "Jane" & dd$day == "2"])))
})

test_that("aggregate_rows averages correctly when mean is provided to fun", {
  expect_equal(aggregate_rows(d = dd,
                              grain = rlang::quo(person),
                              spread = rlang::quo(day),
                              fill = rlang::quo(count),
                              fun = mean)$count,
               c(mean(dd$count[dd$person == "Jack" & dd$day == "1"]),
                 mean(dd$count[dd$person == "Jack" & dd$day == "2"]),
                 mean(dd$count[dd$person == "Jane" & dd$day == "1"]),
                 mean(dd$count[dd$person == "Jane" & dd$day == "2"])))
})

test_that("aggregate_rows errors if fill is character and fun needs numeric", {
  expect_error(aggregate_rows(d = dd,
                              grain = rlang::quo(person),
                              spread = rlang::quo(day),
                              fill = rlang::quo(activity),
                              fun = sum),
               regexp = "character",
               class = "rlang_error")
})

test_that("aggregate_rows takes function(x) and works with character `fill`", {
  expect_equal(
    aggregate_rows(d = dd,
                   grain = rlang::quo(person),
                   spread = rlang::quo(day),
                   fill = rlang::quo(activity),
                   # Find the mode in each group
                   fun = function(x) names(sort(table(x), decreasing = TRUE))[1]
    )$activity,
    c("shower", "shower", "bath", "bath")
  )
})

# Test pivot_maker -------------------------------------------------------------
to_pivot <-
  tibble::tibble(
    rows = factor(c("a", "a", "b")),
    cols = factor(c(1, 2, 1)),
    num_fill = 1:3,
    char_fill = c("one", NA, "three")
  )
num_pivot <-
  tibble::tibble(
    rows = factor(c("a", "b")),
    cols_1 = c(1L, 3L),
    cols_2 = c(2L, NA)
  )
char_pivot <-
  tibble::tibble(
    rows = factor(c("a", "b")),
    cols_1 = c("one", "three"),
    cols_2 = c(NA_character_, NA_character_)
  )

test_that("pivot_maker pivots correctly with defaults", {
  expect_equal(
    pivot_maker(d = to_pivot,
                grain = rlang::quo(rows),
                spread = rlang::quo(cols),
                fill = rlang::quo(num_fill),
                missing_fill = NA),
    num_pivot,
    check.attributes = FALSE
  )
  expect_equal(
    pivot_maker(d = to_pivot,
                grain = rlang::quo(rows),
                spread = rlang::quo(cols),
                fill = rlang::quo(char_fill),
                missing_fill = NA),
    char_pivot,
    check.attributes = FALSE
  )
})

test_that("pivot_maker respects missing_fill", {
  num_pivot$cols_2[is.na(num_pivot$cols_2)] <- 0L
  expect_equal(
    pivot_maker(d = to_pivot,
                grain = rlang::quo(rows),
                spread = rlang::quo(cols),
                fill = rlang::quo(num_fill),
                missing_fill = 0L),
    num_pivot,
    check.attributes = FALSE
  )
})

test_that("pivot works with numeric grain column", {
  d <- data.frame(id = rep(1:3, 2),
                  to_col = rep(letters[1:2], each = 3),
                  vals = rnorm(6))
  expect_setequal(pivot(d, id, to_col, vals)$id, d$id)
})
