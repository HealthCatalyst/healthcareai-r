context("Checking functionality of ROC and PR calculation")

test_that("AU_ROC, TPR, and FPR are calculated correctly", {
  df <- data.frame(a = rep( seq(0,1,by = 0.1), times = 9))
  df[,'b'] <- 0
  df[c(56,62,63,68,74,75,76,81,82,84,85,87,88, seq(90,99,by = 1)),'b'] = 1
  # prepare vectors
  pred <- df[,'a']
  labels <- df[,'b']
  # generate the AUC
  capture.output(output <- generateAUC(predictions = df[,'a'], labels = df[,'b'],aucType = 'SS'))
  expect_equal(output$AUC, 0.5875286, tolerance = 1e-6)
  expect_equal(output$IdealCutoffs[2], 0.6086957, tolerance = 1e-6) 
  expect_equal(output$IdealCutoffs[3], 0.4078947, tolerance = 1e-6)
})

test_that("AU_PR, precision, and recall are calculated correctly", {
  df <- data.frame(a = rep( seq(0,1,by = 0.1), times = 9))
  df[,'b'] <- 0
  df[c(56,62,63,68,74,75,76,81,82,84,85,87,88, seq(90,99,by = 1)),'b'] = 1
  # prepare vectors
  pred <- df[,'a']
  labels <- df[,'b']
  # generate the AUC
  capture.output(output <- generateAUC(predictions = df[,'a'], labels = df[,'b'],aucType = 'PR'))
  expect_equal(output$AUC, 0.2576011, tolerance = 1e-6)
  expect_equal(output$IdealCutoffs[2], 0.2638889, tolerance = 1e-6) 
  expect_equal(output$IdealCutoffs[3], 0.826087, tolerance = 1e-6)
})
