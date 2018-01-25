context("Checking missingness")

test_that("For a given dataframe, function returns expected output", {
  dat <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                    b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                    c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                    d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
  expect_warning( out <- missingness(dat) )
  manual <- sort(100 * sapply(dat, function(x) sum(is.na(x))) / nrow(dat))
  expect_equal(
    out$percent_missing,
    unname(manual)
  )
  expect_equal(
    out$variable,
    names(manual)
  )
})

test_that("For a given matrix, function returns expected output", {
  n <- matrix(c(1, 3, NA, NaN, "NULL", "NAs", "nil", "NONE"),
              nrow = 4, ncol = 2)
  expect_warning( out <- missingness(n) )
  expect_equal(out$percent_missing, c(0, 25))
})

test_that("With user defined NA values, function returns expected output", {
  dat2 <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                     b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                     c = c(10, 5, 8, "void", NA, "NULL", NaN, "Nas"),
                     d = c(1, 6, 7, 8, 9, 5, 10, "what"))
  expect_warning( out <- missingness(dat2, to_search = "void"), "void")
  expect_equal(out$percent_missing, c(0, 0, 12.5, 12.5))
})

test_that("For a given vector, function returns expected output", {
  vect <- c(1, 2, 3, NA)
  expect_equal(missingness(vect)$percent_missing, 25)
})

test_that("Output is of right class", {
  d3 <- data.frame(x = 1:2, y = c("a", "b"))
  expect_true(is.data.frame(missingness(d3)))
  expect_true(is.data.frame(missingness(d3, return_df = TRUE)))
  expect_true(is.vector(missingness(d3, return_df = FALSE)))
})
