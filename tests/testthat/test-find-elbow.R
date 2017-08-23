context("Checking the functionality of findElbow")

test_that("A concave curve gives the correct index of the elbow",{
  y <- c(8.5, 4.9, 2.8, 2.5, 1.9, 1.1, 1.1, 0.9)
  idx <- findElbow(y)
  testthat::expect_equal(idx,3)
})

test_that("A non-concave curve gives correct error",{
  y <- c(6,5.5,5.8,4,2,1.5)
  testthat::expect_error(findElbow(y), # <-- error
                         "Your curve doesn't appear to be concave")
})

