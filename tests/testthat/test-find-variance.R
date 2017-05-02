context("Checking findVariance")

test_that("One cat column and no dates columns give correct result", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9))

  dfRes <- findVariation(df = df,
                         categoricalCols = "Gender",
                         measureCol = "LOS")
  
  expected <- data.frame(DimensionalAttributes = c('Gender','Gender'),
                         CategoriesGrouped = c('M','F'),
                         MeasureCOV = c('LOSCOV|65.52','LOSCOV|58.21'))
  
  testthat::expect_equal(dfRes,expected)
})