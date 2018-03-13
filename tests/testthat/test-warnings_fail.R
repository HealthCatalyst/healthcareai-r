context("expect failure here")

test_that("warnings cause travis to fail", {
  odd <- 1:2 * 1:5
  expect_equal(length(odd), 5)
})
