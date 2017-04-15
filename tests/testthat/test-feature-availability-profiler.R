context('Checking featureAvailabilityProfiler')

test_that("", {
  expect_equal(isBinary(c(1,2,1,1,NA)), TRUE)
  expect_equal(isBinary(c(1,4,NA,1,1)), TRUE)
})