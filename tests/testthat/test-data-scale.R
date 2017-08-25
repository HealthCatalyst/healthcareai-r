context("Checking functionality of dataScale")

test_that("Data set are scaled correctly w/o NA",{
  df <- data.frame(a = c(2,1,3,2,4),b = c(2,8,6,7,9))
  res <- dataScale(df)
  
  mean_vec <- list(a=2.4, b=6.4)
  sd_vec <- list(a=1.140175, b=2.701851)
  scaledDf <- data.frame(a = c(-0.3508232,-1.2278812,0.5262348,-0.3508232,1.4032928),
                        b = c(-1.6285131,0.5921866,-0.1480466,0.2220700,0.9623032))
  expected <- list(mean_vec,sd_vec,scaledDf)
  names(expected) <- c("means", "standard_deviations", "scaled_df")
  
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("Data set are scaled correctly with NA in one column",{
  df <- data.frame(a = c(2,1,3,2,4),b = c(2,NA,6,7,9))
  res <- dataScale(df)
  
  mean_vec <- list(a=2.4, b=6.0)
  sd_vec <- list(a=1.140175, b=2.943920)
  scaledDf <- data.frame(a = c(-0.3508232,-1.2278812,0.5262348,-0.3508232,1.4032928),
                        b = c(-1.3587324,NA,0.0000000,0.3396831,1.0190493))
  expected <- list(mean_vec,sd_vec,scaledDf)
  names(expected) <- c("means", "standard_deviations", "scaled_df")
  
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("Data set are scaled correctly with NA in two columns",{
  df <- data.frame(a = c(2,NA,3,2,NA),b = c(2,NA,6,7,9))
  res <- dataScale(df)
  
  mean_vec <- list(a=2.333333, b=6.000000)
  sd_vec <- list(a=0.5773503, b=2.9439203)
  scaledDf <- data.frame(a = c(-0.5773503,NA,1.1547005,-0.5773503,NA),
                        b = c(-1.3587324,NA,0.0000000,0.3396831,1.0190493))
  expected <- list(mean_vec,sd_vec,scaledDf)
  names(expected) <- c("means", "standard_deviations", "scaled_df")
  
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("Data set are scaled correctly with only NA's in one column",{
  df <- data.frame(a = c(NA,NA,NA,NA,NA),b = c(2,NA,6,7,9))
  res <- dataScale(df)
  
  mean_vec <- list(a=NaN, b=6)
  sd_vec <- list(a=NaN, b=2.94392)
  scaledDf <- data.frame(a = as.numeric(c(NA,NA,NA,NA,NA)),
                        b = c(-1.3587324,NA,0.0000000,0.3396831,1.0190493))
  expected <- list(mean_vec,sd_vec,scaledDf)
  names(expected) <- c("means", "standard_deviations", "scaled_df")
  
  expect_equal(res, expected, tolerance = 1e-6)
})