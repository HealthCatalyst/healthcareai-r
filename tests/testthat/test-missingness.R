context("Checking missingness")

dat <- data.frame(a = c(1, 2, "NA", NA, "none", "??", "?", 5),
                  b = c("blank", 0, "na", "None", "none", 3, 10, 4),
                  c = c(10, 5, 8, 1, NA, "NULL", NaN, "Nas"),
                  d = c(1, 6, 7, 8, 9, 5, 10, 1078950492))
suppressWarnings( out <- missingness(dat) )
suppressWarnings( out_vec <- missingness(dat, return_df = FALSE) )
manual <- sort(100 * sapply(dat, function(x) sum(is.na(x))) / nrow(dat))

test_that("For a given dataframe, function returns expected output", {
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
  expect_s3_class(out, "data.frame")
  expect_s3_class(out, "missingness")
  expect_true(is.numeric(out_vec))
  expect_s3_class(out_vec, "missingness")
})

test_that("Variable names aren't repeated in rownames", {
  out <- missingness(data.frame(x = 1:2, y = c("a", NA)))
  expect_true(length(setdiff(rownames(out), out$variable)) > 0)
})

test_that("missingness class is attached to DF", {
  expect_s3_class(out, "missingness")
})

test_that("plot.missingness is registered generic", {
  expect_true("plot.missingness" %in% methods("plot"))
})

test_that("plot.missingness returns a ggplot", {
  expect_s3_class(plot(out, print = FALSE), "gg")
})

test_that("plot.missingness seems to respect options", {
  def <- plot(out, print = FALSE)
  custom <- plot(out, remove_zeros = TRUE)
  expect_false(isTRUE(all.equal(def, custom)))
})

test_that("plot.missingness works on vector input", {
  expect_s3_class(plot(out_vec, print = FALSE), "gg")
})

test_that("missingness preserves small percents-missing", {
  dd <- data.frame(x = c(NA, 1:9999),
                   y = c(rep(NA, 5), rep(0L, 9995)))
  expect_setequal(missingness(dd)$percent_missing,
                  unname(purrr::map_dbl(dd, ~sum(is.na(.x))) * 100 / nrow(dd)))
})

test_that("all rows of a missingness data frame are printed", {
  dd <- data.frame(id = paste0("id", 1:100),
                   tocol = paste0("v", 1:100),
                   val = 1:100) %>%
    pivot(id, tocol, val)
  print_out <- capture_output( print(missingness(dd)) )
  expect_true(all(stringr::str_detect(print_out, paste0("v", 1:100))))
})

test_that("printing works whether return_df or not", {
  expect_error(capture_output( print(out) ), NA)
  expect_error(capture_output( print(out_vec) ), NA)
})

test_that("plot.missingness respects max_char", {
  md <- missingness(data.frame(long_name = NA, longer_still = 1))
  expect_false(isTRUE(all.equal(plot(md, max_char = 7), plot(md))))
})
