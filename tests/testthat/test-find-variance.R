context("Checking findVariance")

test_that("One cat column and no date columns give correct COV", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9))

  dfRes <- findVariation(df = df,
                         categoricalCols = "Gender",
                         measureCol = "LOS")
  
  expected <- data.frame(DimensionalAttributes = c('Gender','Gender'),
                         CategoriesGrouped = c('M','F'),
                         MeasureCOV = c('LOS|65.52','LOS|58.21'),
                         MeasureVol = c('LOS|3','LOS|3'),
                         MeasureImpact = c('LOS|196.56','LOS|174.63'))
  
  testthat::expect_equal(dfRes,expected)
})


test_that("Two cat columns and no date columns give correct COV", {
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
    MeasureCOV = c('LOS|81.49','LOS|31.04','LOS|65.52',
                   'LOS|58.21','LOS|54.39','LOS|42.04'),
    MeasureVol = c('LOS|4','LOS|2','LOS|3','LOS|3','LOS|2','LOS|2'),
    MeasureImpact = c('LOS|325.96','LOS|62.08','LOS|196.56','LOS|174.63',
                      'LOS|108.78','LOS|84.08'))
  
  # Reset factor levels of both dataframes (as we don't care if they're equal)
  testthat::expect_equal(levels(droplevels(dfRes)),
                         levels(droplevels(expected)))
  
})