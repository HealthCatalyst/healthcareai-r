context('Checking that reasonable hour bins are calculated for profiler')

test_that("Vector created is what's expected for sub-24 hrs", {
  expect_equal(calculateHourBins(19), c(0, 1, 2, 3, 4, 6, 8, 12, 18, 24))
  expect_equal(calculateHourBins(11), c(0, 1, 2, 3, 4, 6, 8, 12, 18, 24))
})

test_that("Vector created is what's expected for after 24 hrs", {
  expect_equal(calculateHourBins(25), c(0, 1, 2, 3, 4, 6, 8, 12, 18, 24, 48))
  expect_equal(calculateHourBins(49), c(0, 1, 2, 3, 4, 6, 8, 
                                        12, 18, 24, 48, 72))
})