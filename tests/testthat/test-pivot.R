context("Checking pivot")

# Setup ------------------------------------------------------------------------
dd <- tibble::tibble(person = as.factor(rep(c("Jack", "Jane"), each = 6)),
                     day = as.factor(rep(1:2, 6)),
                     count = 1:12,
                     activity = rep(c("shower", "shower", "bath"), each = 4))
pivot(dd, person, day, count)

# Test pivot -------------------------------------------------------------------
test_that("pivot fails informatively if any provided columns aren't present", {
            expect_error(pivot(dd, person, group), regexp = "group")
            expect_error(pivot(dd, whatev, day, count), regexp = "whatev")
            expect_error(pivot(dd, person, day, not_here), regexp = "not_here")
            expect_error(pivot(dd, aaa, bbb, ccc), regexp = "aaa")
            expect_error(pivot(dd, aaa, bbb, ccc), regexp = "ccc")
          })

test_that("pivot warns if no fill column or no function provided", {
  expect_warning(pivot(dd, person, day))
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
               regexp = "character")
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

test_that("aggregate_rows fills in NAs where no instances present", {
  
})
dd$person <- forcats::fct_expand(dd$person, "Mark")
