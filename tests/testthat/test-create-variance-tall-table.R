context("Checking create variance tall table")

test_that("Two row input df provides correct output", {
  df <- data.frame(LactateOrderProvSpecDSC = c("Pulmonary Disease",
                                               "Family Medicine"),
                   LactateOrderProvNM = c("Hector Salamanca",
                                          "Gus Fring"),
                   COV = c(0.43,0.35),
                   VolumeRaw = c(2,3),
                   VolumePercent = c(0.32,0.78),
                   Impact = c(0.46,1.05),
                   AboveMeanCOVFLG = c('Y','N'),
                   AboveMeanVolumeFLG = c('N','Y'))
  
  categoricalCols <- c("LactateOrderProvSpecDSC","LactateOrderProvNM")
  
  dfRes <- createVarianceTallTable(df = df, 
                                   categoricalCols = categoricalCols, 
                                   measure = "LOS")
  
  expected <- 
    data.frame(
      DimensionalAttributes = c('LactateOrderProvSpecDSC|LactateOrderProvNM',
                                'LactateOrderProvSpecDSC|LactateOrderProvNM'),
      CategoriesGrouped = c('Pulmonary Disease|Hector Salamanca',
                            'Family Medicine|Gus Fring'),
      MeasureCOV = c('LOS|0.43','LOS|0.35'),
      MeasureVolumeRaw = c('LOS|2','LOS|3'),
      MeasureVolumePercent = c('LOS|0.32','LOS|0.78'),
      MeasureImpact = c('LOS|0.46','LOS|1.05'),
      AboveMeanCOVFLG = c('LOS|Y','LOS|N'),
      AboveMeanVolumeFLG = c('LOS|N','LOS|Y'),
      stringsAsFactors = FALSE)
  
  testthat::expect_equal(dfRes,expected)
})

test_that("Proper error is raised w/ incorrect categorical col input", {
  df <- data.frame(LactateOrderProvSpecDSC = c("Pulmonary Disease",
                                               "Family Medicine"),
                   LactateOrderProvNM = c("Hector Salamanca",
                                          "Gus Fring"),
                   COV = c(0.43,0.35),
                   VolumeRaw = c(2,3),
                   VolumePercent = c(0.32,0.78),
                   Impact = c(0.46,1.05),
                   AboveMeanCOVFLG = c('Y','N'),
                   AboveMeanVolumeFLG = c('N','Y'))
  
  categoricalCols <- c("LactateOrderProvSpecDS", #<-- error
                       "LactateOrderProvNM")
  
  testthat::expect_error(
              createVarianceTallTable(df = df, 
                                      categoricalCols = categoricalCols, 
                                      measure = "LOS"),
              'One of the categoricalCols is not in the df')
})

test_that("Proper error is raised w/ incorrect non-categorical col input", {
  df <- data.frame(LactateOrderProvSpecDSC = c("Pulmonary Disease",
                                               "Family Medicine"),
                   LactateOrderProvNM = c("Hector Salamanca",
                                          "Gus Fring"),
                   CO = c(0.43,0.35), # <-- error
                   VolumeRaw = c(2,3),
                   VolumePercent = c(0.32,0.78),
                   Impact = c(0.46,1.05),
                   AboveMeanCOVFLG = c('Y','N'),
                   AboveMeanVolumeFLG = c('N','Y'))
  
  categoricalCols <- c("LactateOrderProvSpecDSC","LactateOrderProvNM")
  
  testthat::expect_error(
    createVarianceTallTable(df = df, 
                            categoricalCols = categoricalCols, 
                            measure = "LOS"),
    'One of the required columns is not in the df')
})

test_that("Proper error is raised when measure is in categoricalCols", {
  df <- data.frame(LactateOrderProvSpecDSC = c("Pulmonary Disease",
                                               "Family Medicine"),
                   LactateOrderProvNM = c("Hector Salamanca",
                                          "Gus Fring"),
                   CO = c(0.43,0.35),
                   VolumeRaw = c(2,3),
                   VolumePercent = c(0.32,0.78),
                   Impact = c(0.46,1.05),
                   AboveMeanCOVFLG = c('Y','N'),
                   AboveMeanVolumeFLG = c('N','Y'))
  
  categoricalCols <- c("LactateOrderProvSpecDSC","LactateOrderProvNM")
  
  testthat::expect_error(
    createVarianceTallTable(df = df, 
                            categoricalCols = categoricalCols, 
                            measure = "LactateOrderProvNM"), # <-- error
    'Your measure cannot also be listed in categoricalCols')
})