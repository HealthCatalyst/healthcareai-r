context("Checking variationAcrossGroups")

test_that("One cat column, one measure col, and no date columns give correct df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
  
  dfRes <- variationAcrossGroups(df = df,
                                 categoricalCols = "Gender",
                                 measureColumn = "LOS")
  
  dfRes <- data.frame(lapply(dfRes, as.character))
  
  expected <- data.frame(measure = c('LOS','LOS'),
                         group = c('F','M'),
                         Mean = c('5.3','2.9'),
                         Std = c('2.57','1.90'),
                         SE = c('2.06','1.53'),
                         Min = c('3.2', '1.3'),
                         Q1 = c('3.80', '1.85'),
                         Median = c('4.5','2.4'),
                         Q3 = c('6.0', '3.7'),
                         Max = c('9','5'))
  
  # Drop row names, so the two dfs can match w/o specifying row names explicitly
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
})

test_that("One cat column, one measure col, and no date col and NA in cat col 
          give correct df", {
            df <- data.frame(Gender = c('F','M','M','M','M','F',NA,'F'),
                             LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
            
            dfRes <- variationAcrossGroups(df = df,
                                           categoricalCols = "Gender",
                                           measureColumn = "LOS")
            
            dfRes <- data.frame(lapply(dfRes, as.character), stringsAsFactors = FALSE)
            
            expected <- data.frame(measure = c('LOS','LOS'),
                                   group = c('F','M'),
                                   Mean = c('4.07','2.90'),
                                   Std = c('0.902','1.900'),
                                   SE = c('4.51','1.53'),
                                   Min = c('3.2', '1.3'),
                                   Q1 = c('3.60', '1.85'),
                                   Median = c('4.0','2.4'),
                                   Q3 = c('4.5', '3.7'),
                                   Max = c('5','5'),
                                   stringsAsFactors = FALSE)
            
            rownames(dfRes) <- c()
            rownames(expected) <- c()
            
            testthat::expect_equal(dfRes,expected)
            })

test_that("Two cat columns and no date columns give correct df", {
  set.seed(35)
  df <- data.frame(Gender = factor(rbinom(100, 1, 0.45), labels = c("Male","Female")), 
                   Dept = sample(c("A","B","C"), 50, replace = T, prob = c(0.2,0.3,0.5)),
                   LOS = c(rnorm(30),rnorm(70,10,5))
                   )
  
  categoricalCols <- c("Dept","Gender")
  
  dfRes <- variationAcrossGroups(df = df, 
                                 categoricalCols = categoricalCols,
                                 measureColumn = "LOS")
  
  dfRes <- data.frame(lapply(dfRes, as.character), stringsAsFactors = FALSE)
  
  expected <- data.frame(measure = c('LOS','LOS','LOS','LOS','LOS','LOS'),
                         group = c('B.Female','C.Male','A.Female','C.Female','A.Male','B.Male'),
                         Mean = c(' 8.27',' 6.80',' 7.31',' 8.44',' 8.53','12.03'),
                         Std = c('7.27','6.91','7.21','6.86','6.03','7.19'),
                         SE = c('1.138','0.984','1.014','1.230','1.415','1.673'),
                         Min = c('-1.568', '-1.844',' 0.221','-0.676',' 0.315','-0.616'),
                         Q1 = c('1.172', '0.497','0.854','1.227','5.344','9.191'),
                         Median = c(' 8.66',' 6.28',' 3.27',' 7.47',' 9.81','12.07'),
                         Q3 = c('14.6', '12.7','14.9','15.4','11.1','15.9'),
                         Max = c('19.6','20.6','16.6','19.2','19.3','23.1'),
                         stringsAsFactors = FALSE)
  
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
  
})

test_that("One cat col, one measure col, and one date col give correct df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F','M','F','F','F','M',
                              'F','F','F','M','M','F','M'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5,2,3,5,2,6,4,8,4,6.5,3.8,2,1.8),
                   StartDTS = c('2012-01-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-02 10:04:23',
                                '2012-01-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-03-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-03-05 10:04:23', '2012-03-01 10:04:23',
                                '2012-02-06 10:04:23', '2012-02-02 10:04:23',
                                '2012-02-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-08 10:04:23',
                                '2012-03-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-01-01 10:04:23', '2012-01-01 10:04:23'))
  
  dfRes <- variationAcrossGroups(df = df,
                         categoricalCols = "Gender",
                         measureColumn = "LOS",
                         dateCol = 'StartDTS')
  
  dfRes <- data.frame(lapply(dfRes, as.character), stringsAsFactors = FALSE)
  
  expected <- data.frame(measure = c('LOS','LOS','LOS','LOS','LOS','LOS'),
                         group = c('F.2012/01','M.2012/02','M.2012/01','F.2012/03','M.2012/03','F.2012/02'),
                         Mean = c('3.40','4.10','2.10','5.00','4.10','4.75'),
                         Std = c('1.510','2.476','0.424','2.708','2.265','2.500'),
                         SE = c('2.25','1.66','4.95','1.85','1.81','1.90'),
                         Min = c('2.0', '1.3','1.8','3.0','2.0','2.0'),
                         Q1 = c('2.60', '3.15','1.95','3.75','2.90','3.50'),
                         Median = c('3.2','5.0','2.1','4.0','3.8','4.5'),
                         Q3 = c('4.10', '5.50','2.25','5.25','5.15','5.75'),
                         Max = c('5.0','6.0','2.4','9.0','6.5','8.0'),
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
            
            dfRes <- variationAcrossGroups(df = df,
                                   categoricalCols = "Gender",
                                   measureColumn = c("LOS","BP"))
            
            dfRes <- data.frame(lapply(dfRes, as.character), stringsAsFactors = FALSE)
            
            expected <- data.frame(measure = c('LOS','LOS','BP','BP'),
                                   group = c('F','M','F','M'),
                                   Mean = c('  5.3','  2.9','121.5','109.7'),
                                   Std = c(' 2.57',' 1.90','44.98','34.93'),
                                   SE = c('2.06','1.53','2.70','3.14'),
                                   Min = c(' 3.2', ' 1.3','58.0','89.0'),
                                   Q1 = c('  3.80', '  1.85','106.75',' 89.50'),
                                   Median = c('  4.5','  2.4','134.0',' 90.0'),
                                   Q3 = c('  6.0', '  3.7','148.8','120.0'),
                                   Max = c('  9','  5','160','150'),
                                   stringsAsFactors = FALSE)
            
            
            rownames(dfRes) <- c()
            rownames(expected) <- c()
            
            # Reset factor levels of both dataframes (as we don't care if they're equal)
            testthat::expect_equal(dfRes,expected)
            })

test_that("One cat col, two measure cols, and one date col give correct df", {
  
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F','M','F','F','F','M',
                              'F','F','F','M','M','F','M'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5,2,3,5,2,6,4,8,4,6.5,3.8,2,1.8),
                   BP = c(123,129,89,150,90,58,160,145,132,156,86,152,98,38,130,125,
                          38,168,125,173),
                   StartDTS = c('2012-01-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-02 10:04:23',
                                '2012-01-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-03-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-03-05 10:04:23', '2012-03-01 10:04:23',
                                '2012-02-06 10:04:23', '2012-02-02 10:04:23',
                                '2012-02-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-08 10:04:23',
                                '2012-03-01 10:04:23', '2012-03-01 10:04:23',
                                '2012-01-01 10:04:23', '2012-01-01 10:04:23'))

  
  dfRes <- variationAcrossGroups(df = df,
                         categoricalCols = "Gender",
                         measureColumn = c("LOS","BP"),
                         dateCol = 'StartDTS')
  
  dfRes <- data.frame(lapply(dfRes, as.character), stringsAsFactors = FALSE)
  
  expected <- data.frame(measure = c('LOS','LOS','LOS','LOS','LOS','LOS',
                                     'BP','BP','BP','BP','BP','BP'),
                         group = c('F.2012/01','M.2012/02','M.2012/01','F.2012/03','M.2012/03','F.2012/02',
                                   'F.2012/01','M.2012/02','M.2012/01','F.2012/03','M.2012/03','F.2012/02'),
                         Mean = c('  3.40','  4.10','  2.10','  5.00','  4.10','  4.75',
                                  '131.00','112.33','131.50','103.00','112.67','123.25'),
                         Std = c(' 1.510',' 2.476',' 0.424',' 2.708',' 2.265',' 2.500',
                                 '12.166','32.929','58.690','64.052','67.122','27.464'),
                         SE = c(' 2.25',' 1.66',' 4.95',' 1.85',' 1.81',' 1.90',
                                '10.77',' 3.41',' 2.24',' 1.61',' 1.68',' 4.49'),
                         Min = c('  2.0', '  1.3','  1.8','  3.0','  2.0','  2.0',
                                 '123.0',' 89.0',' 90.0',' 38.0',' 38.0',' 86.0'),
                         Q1 = c('  2.60', '  3.15','  1.95','  3.75','  2.90','  3.50',
                                '124.00',' 93.50','110.75',' 53.00',' 85.00','115.25'),
                         Median = c('  3.2','  5.0','  2.1','  4.0','  3.8','  4.5',
                                    '125.0',' 98.0','131.5','107.0','132.0','127.5'),
                         Q3 = c('  4.10', '  5.50','  2.25','  5.25','  5.15','  5.75',
                                '135.00','124.00','152.25','157.00','150.00','135.50'),
                         Max = c('  5.0','  6.0','  2.4','  9.0','  6.5','  8.0',
                                 '145.0','150.0','173.0','160.0','168.0','152.0'),
                         stringsAsFactors = FALSE)
  
  rownames(dfRes) <- c()
  rownames(expected) <- c()
  
  testthat::expect_equal(dfRes,expected)
})

test_that("Measure col missing from df gives correct error (when one 
          specified)", {
            df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                             LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
            
            testthat::expect_error(variationAcrossGroups(df = df,
                                                 categoricalCols = "Gender",
                                                 measureColumn = "LO"), # <-- error
                                   "The measure column or one of the categorical cols is")
            })

test_that("Measure col missing from df gives correct error (when two 
          specified)", {
            df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                             LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
            
            testthat::expect_error(variationAcrossGroups(df = df,
                                                 categoricalCols = "Gender",
                                                 measureColumn = c("LOS","BP")), # <-- error
                                   "The measure column or one of the categorical cols is")
            })

test_that("Categorical col missing from df gives correct error (when one 
          specified)", {
            df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                             LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
            
            testthat::expect_error(variationAcrossGroups(df = df,
                                                 categoricalCols = "Gendr", # <-- error
                                                 measureColumn = "LOS"),
                                   "The measure column or one of the categorical cols is")
            })

test_that("Categorical col missing from df gives correct error (when two 
          specified)", {
            df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                             LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                             Age = c(23,45,63,42,78,32,15,65))
            
            testthat::expect_error(variationAcrossGroups(df = df,
                                                 categoricalCols = c("Gender"),
                                                 measureColumn = c("LOS","Ag")),
                                   "The measure column or one of the categorical cols is")
            })

test_that("Measure col as strings gives correct error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   Married = c('Y','N','Y','N','N','Y','N','Y'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
  
  testthat::expect_error(variationAcrossGroups(df = df,
                                       categoricalCols = "Gender",
                                       measureColumn = "Married"), # <-- error
                         "measureColumn needs to be of class numeric or int")
})

test_that("Categorical col as numbers gives correct error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   Age = c(23,45,63,42,78,32,15,65))
  
  testthat::expect_error(variationAcrossGroups(df = df,
                                       categoricalCols = c("Gender","Age"),
                                       measureColumn = "LOS"),
                         "categoricalCols cannot be of class numeric or int")
})

#################################################################################

test_that("Date column in wrong format gives correct error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   AdmitDTS = c('01-2012 12:34:12','2012-01-01 12:34:12',
                                '2012-01-01 12:34:12','2012-01-01 12:34:12',
                                '2012-01-01 12:34:12','2012-01-01 12:34:12',
                                '2012-01-01 12:34:12','2012-01-01 12:34:12'))
  
  testthat::expect_error(variationAcrossGroups(df = df,
                                       categoricalCols = c("Gender"),
                                       measureColumn = "LOS",
                                       dateCol = "AdmitDTS"), # <-- error
                         paste0("AdmitDTS may not be a datetime column, or the",
                                " column may not be in format YYYY-MM-DD"))
})

test_that("Date column passed as number gives correct error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   Age = c(23,45,63,42,78,32,15,65))
  
  testthat::expect_error(variationAcrossGroups(df = df,
                                       categoricalCols = c("Gender"),
                                       measureColumn = "LOS",
                                       dateCol = "Age"), # <-- error
                         paste0("Age may not be a datetime column, or the",
                                " column may not be in format YYYY-MM-DD"))
})
