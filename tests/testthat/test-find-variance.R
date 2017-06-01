context("Checking findVariance")

test_that("One cat column, one measure col, and no date columns give correct df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5))

  dfRes <- findVariation(df = df,
                         categoricalCols = "Gender",
                         measureCol = "LOS")
  
  expected <- data.frame(DimensionalAttributes = c('Gender','Gender'),
                         CategoriesGrouped = c('M','F'),
                         MeasureCOV = c('LOS|0.66','LOS|0.49'),
                         MeasureVolumeRaw = c('LOS|3','LOS|4'),
                         MeasureVolumePercent = c('LOS|0.43','LOS|0.57'),
                         MeasureImpact = c('LOS|1.98','LOS|1.96'),
                         AboveMeanCOVFLG = c('LOS|Y','LOS|N'),
                         AboveMeanVolumeFLG = c('LOS|N','LOS|Y'),
                         # findVariation returns strings as characters
                         stringsAsFactors = FALSE)
  
  # Drop row names, so the two dfs can match w/o specifying row names explicitly
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
})

test_that("One cat column, one measure col, and no date col and NA in cat col 
           give correct df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F',NA,'F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
  
  dfRes <- findVariation(df = df,
                         categoricalCols = "Gender",
                         measureCol = "LOS")
  
  expected <- data.frame(DimensionalAttributes = c('Gender','Gender'),
                         CategoriesGrouped = c('M','F'),
                         MeasureCOV = c('LOS|0.66','LOS|0.22'),
                         MeasureVolumeRaw = c('LOS|3','LOS|3'),
                         MeasureVolumePercent = c('LOS|0.5','LOS|0.5'),
                         MeasureImpact = c('LOS|1.98','LOS|0.66'),
                         AboveMeanCOVFLG = c('LOS|Y','LOS|N'),
                         AboveMeanVolumeFLG = c('LOS|N','LOS|N'),
                         stringsAsFactors = FALSE)
  
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
})

test_that("Two cat columns and no date columns give correct df", {
  df <- data.frame(Dept = c('A','A','A','B','B','B','B'),
                   Gender = c('F','M','M','M','M','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9))
  
  categoricalCols <- c("Dept","Gender")
  
  dfRes <- findVariation(df = df, 
                         categoricalCols = categoricalCols,
                         measureCol = "LOS")
  
  expected <- data.frame(
    DimensionalAttributes = c('Dept','Dept','Gender','Gender',
                              'Dept|Gender','Dept|Gender'),
    CategoriesGrouped = c('B','A','M','F','B|F','B|M'),
    MeasureCOV = c('LOS|0.81','LOS|0.31','LOS|0.66',
                   'LOS|0.58','LOS|0.54','LOS|0.42'),
    MeasureVolumeRaw = c('LOS|4','LOS|2','LOS|3','LOS|3','LOS|2','LOS|2'),
    MeasureVolumePercent = c('LOS|0.67','LOS|0.33','LOS|0.5','LOS|0.5',
                             'LOS|0.5','LOS|0.5'),
    MeasureImpact = c('LOS|3.24','LOS|0.62','LOS|1.98','LOS|1.74',
                      'LOS|1.08','LOS|0.84'),
    AboveMeanCOVFLG = c('LOS|Y','LOS|N','LOS|Y','LOS|N',
                        'LOS|Y','LOS|N'),
    AboveMeanVolumeFLG = c('LOS|Y','LOS|N','LOS|N','LOS|N',
                           'LOS|Y','LOS|Y'),
    stringsAsFactors = FALSE)
  
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
  
})

test_that("One cat col, one measure col, and one date col give correct df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   StartDTS = c('2012-01-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-02 10:04:23',
                                '2012-01-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-04-01 10:04:23', '2012-04-01 10:04:23'))
  
  dfRes <- findVariation(df = df,
                         categoricalCols = "Gender",
                         measureCol = "LOS",
                         dateCol = 'StartDTS')
  
  expected <- data.frame(
    DimensionalAttributes = c('Gender','Gender',
                              'StartDTS','StartDTS','StartDTS',
                              'Gender|StartDTS','Gender|StartDTS'),
    CategoriesGrouped = c('M','F',
                          '2012-02','2012-04','2012-01',
                          'M|2012-02','F|2012-04'),
    MeasureCOV = c('LOS|0.66','LOS|0.49',
                   'LOS|0.83','LOS|0.4','LOS|0.2',
                   'LOS|0.83','LOS|0.4'),
    MeasureVolumeRaw = c('LOS|3','LOS|4',
                         'LOS|2','LOS|2','LOS|2',
                         'LOS|2','LOS|2'),
    MeasureVolumePercent = c('LOS|0.43','LOS|0.57',
                             'LOS|0.33','LOS|0.33','LOS|0.33',
                             'LOS|0.5','LOS|0.5'),
    MeasureImpact = c('LOS|1.98','LOS|1.96',
                      'LOS|1.66','LOS|0.8','LOS|0.4',
                      'LOS|1.66','LOS|0.8'),
    AboveMeanCOVFLG = c('LOS|Y','LOS|N',
                        'LOS|Y','LOS|N','LOS|N',
                        'LOS|Y','LOS|N'),
    AboveMeanVolumeFLG = c('LOS|N','LOS|Y',
                           'LOS|Y','LOS|Y','LOS|Y',
                           'LOS|Y','LOS|Y'),
    stringsAsFactors = FALSE)
  
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
})

test_that("One cat column, two measure col, and no date columns give 
           correct df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   BP = c(123,129,89,150,90,58,160,145))
  
  dfRes <- findVariation(df = df,
                         categoricalCols = "Gender",
                         measureCol = c("LOS","BP"))
  
  expected <- data.frame(DimensionalAttributes = c('Gender','Gender',
                                                   'Gender','Gender'),
                         CategoriesGrouped = c('M','F',
                                               'F','M'),
                         MeasureCOV = c('LOS|0.66','LOS|0.49',
                                        'BP|0.37','BP|0.26'),
                         MeasureVolumeRaw = c('LOS|3','LOS|4',
                                              'BP|4','BP|4'),
                         MeasureVolumePercent = c('LOS|0.43','LOS|0.57',
                                                  'BP|0.5','BP|0.5'),
                         MeasureImpact = c('LOS|1.98','LOS|1.96',
                                           'BP|1.48','BP|1.04'),
                         AboveMeanCOVFLG = c('LOS|Y','LOS|N',
                                             'BP|Y','BP|N'),
                         AboveMeanVolumeFLG = c('LOS|N','LOS|Y',
                                                'BP|N','BP|N'),
                         stringsAsFactors = FALSE)
  
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  # Reset factor levels of both dataframes (as we don't care if they're equal)
  testthat::expect_equal(dfRes,expected)
})

test_that("One cat col, two measure cols, and one date col give correct df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   BP = c(123,129,89,150,90,58,160,145),
                   StartDTS = c('2012-01-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-02 10:04:23',
                                '2012-01-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-04-01 10:04:23', '2012-04-01 10:04:23'))
  
  dfRes <- findVariation(df = df,
                         categoricalCols = "Gender",
                         measureCol = c("LOS","BP"),
                         dateCol = 'StartDTS')
  
  expected <- data.frame(
    DimensionalAttributes = c('Gender','Gender','Gender','Gender',
                              'StartDTS','StartDTS','StartDTS',
                              'StartDTS','StartDTS','StartDTS',
                              'Gender|StartDTS','Gender|StartDTS',
                              'Gender|StartDTS','Gender|StartDTS',
                              'Gender|StartDTS'),
    CategoriesGrouped = c('M','F','F','M',
                          '2012-02','2012-04','2012-02',
                          '2012-01','2012-01','2012-04',
                          'M|2012-02','F|2012-04','M|2012-02',
                          'M|2012-01','F|2012-04'),
    MeasureCOV = c('LOS|0.66','LOS|0.49',
                   'BP|0.37','BP|0.26',
                   'LOS|0.83','LOS|0.4',
                   'BP|0.36','BP|0.18',
                   'LOS|0.2',
                   'BP|0.07',
                   'LOS|0.83','LOS|0.4',
                   'BP|0.36','BP|0.25','BP|0.07'),
    MeasureVolumeRaw = c('LOS|3','LOS|4',
                         'BP|4','BP|4',
                         'LOS|2','LOS|2',
                         'BP|2','BP|3',
                         'LOS|2',
                         'BP|2',
                         'LOS|2','LOS|2',
                         'BP|2','BP|2','BP|2'),
    MeasureVolumePercent = c('LOS|0.43','LOS|0.57',
                             'BP|0.5','BP|0.5',
                             'LOS|0.33','LOS|0.33',
                             'BP|0.29','BP|0.43',
                             'LOS|0.33',
                             'BP|0.29',
                             'LOS|0.5','LOS|0.5',
                             'BP|0.33','BP|0.33','BP|0.33'),
    MeasureImpact = c('LOS|1.98','LOS|1.96',
                      'BP|1.48','BP|1.04',
                      'LOS|1.66','LOS|0.8',
                      'BP|0.72','BP|0.54',
                      'LOS|0.4',
                      'BP|0.14',
                      'LOS|1.66','LOS|0.8',
                      'BP|0.72','BP|0.5','BP|0.14'),
    AboveMeanCOVFLG = c('LOS|Y','LOS|N',
                        'BP|Y','BP|N',
                        'LOS|Y','LOS|N',
                        'BP|Y','BP|N',
                        'LOS|N',
                        'BP|N',
                        'LOS|Y','LOS|N',
                        'BP|Y','BP|Y','BP|N'),
    AboveMeanVolumeFLG = c('LOS|N','LOS|Y',
                           'BP|N','BP|N',
                           'LOS|Y','LOS|Y',
                           'BP|N','BP|Y',
                           'LOS|Y',
                           'BP|N',
                           'LOS|Y','LOS|Y',
                           'BP|Y','BP|Y','BP|Y'),
    stringsAsFactors = FALSE)
  
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
})

test_that("One cat col, two measure cols,one date col, & threshold give correct 
           df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   BP = c(123,129,89,150,90,58,160,145),
                   StartDTS = c('2012-01-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-02 10:04:23',
                                '2012-01-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-04-01 10:04:23', '2012-04-01 10:04:23'))
  
  dfRes <- findVariation(df = df,
                         categoricalCols = "Gender",
                         measureCol = c("LOS","BP"),
                         dateCol = 'StartDTS',
                         threshold = 1.0)
  
  expected <- data.frame(
    DimensionalAttributes = c('Gender','Gender','Gender','Gender',
                              'StartDTS',
                              'Gender|StartDTS'),
    CategoriesGrouped = c('M','F','F','M',
                          '2012-02',
                          'M|2012-02'),
    MeasureCOV = c('LOS|0.66','LOS|0.49','BP|0.37','BP|0.26',
                   'LOS|0.83',
                   'LOS|0.83'),
    MeasureVolumeRaw = c('LOS|3','LOS|4',
                         'BP|4','BP|4',
                         'LOS|2',
                         'LOS|2'),
    MeasureVolumePercent = c('LOS|0.43','LOS|0.57','BP|0.5','BP|0.5',
                             'LOS|0.33',
                             'LOS|0.5'),
    MeasureImpact = c('LOS|1.98','LOS|1.96','BP|1.48','BP|1.04',
                      'LOS|1.66',
                      'LOS|1.66'),
    AboveMeanCOVFLG = c('LOS|Y','LOS|N','BP|Y','BP|N',
                        'LOS|Y','LOS|Y'),
    AboveMeanVolumeFLG = c('LOS|N','LOS|Y','BP|N','BP|N',
                           'LOS|Y',
                           'LOS|Y'),
    stringsAsFactors = FALSE)
  
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
})

test_that("Measure col missing from df gives correct error (when one 
           specified)", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
  
  testthat::expect_error(findVariation(df = df,
                                       categoricalCols = "Gender",
                                       measureCol = "LO"), # <-- error
                         "The measure column or one of the categorical cols is")
})

test_that("Measure col missing from df gives correct error (when two 
           specified)", {
             df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                              LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
             
 testthat::expect_error(findVariation(df = df,
                                      categoricalCols = "Gender",
                                      measureCol = "LO"), # <-- error
                        "The measure column or one of the categorical cols is")
})

test_that("Categorical col missing from df gives correct error (when one 
           specified)", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
  
  testthat::expect_error(findVariation(df = df,
                                       categoricalCols = "Gendr", # <-- error
                                       measureCol = "LOS"),
                         "The measure column or one of the categorical cols is")
})

test_that("Categorical col missing from df gives correct error (when two 
           specified)", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   Age = c(23,45,63,42,78,32,15,65))
  
  testthat::expect_error(findVariation(df = df,
                                       categoricalCols = c("Gender"),
                                       measureCol = c("LOS","Ag")),
                         "The measure column or one of the categorical cols is")
})

test_that("Measure col as strings gives correct error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   Married = c('Y','N','Y','N','N','Y','N','Y'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
  
  testthat::expect_error(findVariation(df = df,
                                       categoricalCols = "Gender",
                                       measureCol = "Married"), # <-- error
                         "measureColumn needs to be of class numeric or int")
})

test_that("Categorical col as numbers gives correct error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   Age = c(23,45,63,42,78,32,15,65))
  
  testthat::expect_error(findVariation(df = df,
                                       categoricalCols = c("Gender","Age"),
                                       measureCol = "LOS"),
                         "categoricalCols cannot be of class numeric or int")
})

test_that("Date column in wrong format gives correct error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   AdmitDTS = c('01-2012 12:34:12','2012-01-01 12:34:12',
                                '2012-01-01 12:34:12','2012-01-01 12:34:12',
                                '2012-01-01 12:34:12','2012-01-01 12:34:12',
                                '2012-01-01 12:34:12','2012-01-01 12:34:12'))
  
  testthat::expect_error(findVariation(df = df,
                                       categoricalCols = c("Gender"),
                                       measureCol = "LOS",
                                       dateCol = "AdmitDTS"), # <-- error
                         paste0("AdmitDTS may not be a datetime column, or the",
                                " column may not be in format YYYY-MM-DD"))
})

test_that("Date column passed as number gives correct error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   Age = c(23,45,63,42,78,32,15,65))
  
  testthat::expect_error(findVariation(df = df,
                                       categoricalCols = c("Gender"),
                                       measureCol = "LOS",
                                       dateCol = "Age"), # <-- error
                         paste0("Age may not be a datetime column, or the",
                                " column may not be in format YYYY-MM-DD"))
})
