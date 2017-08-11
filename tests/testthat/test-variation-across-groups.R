context("Checking variationAcrossGroups")

test_that("One cat column, one measure col, and no date columns give correct df", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
  
  dfRes <- variationAcrossGroups(df = df,
                                 categoricalCols = "Gender",
                                 measureColumn = "LOS")
  
  dfRes[[1]] <- data.frame(lapply(dfRes[[1]], as.character), stringsAsFactors = FALSE)
  dfRes[[2]] <- data.frame(lapply(dfRes[[2]], as.character), stringsAsFactors = FALSE)
  
  df1 <- data.frame(Measure = 'LOS',
                    Groups = 'F-M', 
                    p_value = '0.234984',
                    stringsAsFactors = FALSE)
  
  df2 <- data.frame(measure = c('LOS','LOS'),
                    group = c('F','M'),
                    Mean = c('5.3','2.9'),
                    Std = c('2.57','1.90'),
                    COV = c('0.486','0.655'),
                    Min = c('3.2', '1.3'),
                    Q1 = c('3.80', '1.85'),
                    Median = c('4.5','2.4'),
                    Q3 = c('6.0', '3.7'),
                    Max = c('9','5'),
                    VolumnRaw = c('4','3'),
                    Impact = c('1.94','1.97'),
                    stringsAsFactors = FALSE)
  
  expected <- list(df1,df2)
  names(expected) <- c("P value for each pair of groups", "Basic statistics of each group")
    
    
  
  # Drop row names, so the two dfs can match w/o specifying row names explicitly
  rownames(dfRes[[1]]) <- c(); rownames(dfRes[[2]]) <- c()
  rownames(expected[[1]]) <- c(); rownames(expected[[2]]) <- c()
  
  testthat::expect_equal(dfRes,expected)
})

test_that("One cat column, one measure col, and no date col and NA in cat col 
          give correct df", {
            df <- data.frame(Gender = c('F','M','M','M','M','F',NA,'F'),
                             LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
            
            dfRes <- variationAcrossGroups(df = df,
                                           categoricalCols = "Gender",
                                           measureColumn = "LOS")
            
            dfRes[[1]] <- data.frame(lapply(dfRes[[1]], as.character), 
                                     stringsAsFactors = FALSE)
            dfRes[[2]] <- data.frame(lapply(dfRes[[2]], as.character), 
                                     stringsAsFactors = FALSE)
            
            df1 <- data.frame(Measure = 'LOS',
                              Groups = 'F-M', 
                              p_value = '0.391063',
                              stringsAsFactors = FALSE)
            
            df2 <- data.frame(measure = c('LOS','LOS'),
                              group = c('F','M'),
                              Mean = c('4.07','2.90'),
                              Std = c('0.902','1.900'),
                              COV = c('0.222','0.655'),
                              Min = c('3.2', '1.3'),
                              Q1 = c('3.60', '1.85'),
                              Median = c('4.0','2.4'),
                              Q3 = c('4.5', '3.7'),
                              Max = c('5','5'),
                              VolumnRaw = c('3','3'),
                              Impact = c('0.665','1.966'),
                              stringsAsFactors = FALSE)
            
            expected <- list(df1,df2)
            names(expected) <- c("P value for each pair of groups", 
                                 "Basic statistics of each group")
          
            
            # Drop row names, so the two dfs can match w/o specifying row names explicitly
            rownames(dfRes[[1]]) <- c(); rownames(dfRes[[2]]) <- c()
            rownames(expected[[1]]) <- c(); rownames(expected[[2]]) <- c()
            
            testthat::expect_equal(dfRes,expected)
            })

test_that("Two cat columns and no date columns give correct df", {
  set.seed(35)
  df <- data.frame(Gender = factor(rbinom(100, 1, 0.45), labels = c("Male","Female")), 
                   Dept = sample(c("A","B","C"), 50, replace = TRUE, prob = c(0.2,0.3,0.5)),
                   LOS = c(rnorm(30),rnorm(70,10,5))
                   )
  
  categoricalCols <- c("Dept","Gender")
  
  dfRes <- variationAcrossGroups(df = df, 
                                 categoricalCols = categoricalCols,
                                 measureColumn = "LOS")
  
  dfRes[[1]] <- data.frame(lapply(dfRes[[1]], as.character), 
                           stringsAsFactors = FALSE)
  dfRes[[2]] <- data.frame(lapply(dfRes[[2]], as.character), 
                           stringsAsFactors = FALSE)
  
  df1 <- data.frame(Measure = c('LOS','LOS','LOS','LOS','LOS','LOS',
                                'LOS','LOS','LOS','LOS','LOS','LOS',
                                'LOS','LOS','LOS'),
                    Groups = c("B.Male-C.Male","B.Male-A.Female","B.Male-C.Female",  
                               "B.Male-B.Female","B.Male-A.Male","C.Female-C.Male",  
                               "A.Male-C.Male","B.Female-C.Male","C.Female-A.Female",
                               "A.Male-A.Female","B.Female-A.Female","A.Female-C.Male",  
                               "A.Male-B.Female","C.Female-B.Female","A.Male-C.Female"), 
                    p_value = c("0.401138","0.686254","0.787918","0.841090","0.902915",
                                "0.941093","0.985462","0.988487","0.997409","0.998782",
                                "0.999447","0.999937","0.999999","1.000000","1.000000"),
                    stringsAsFactors = FALSE)
  

  
  df2 <- data.frame(measure = c('LOS','LOS','LOS','LOS','LOS','LOS'),
                    group = c('B.Female','C.Male','A.Female','C.Female','A.Male','B.Male'),
                    Mean = c(' 8.27',' 6.80',' 7.31',' 8.44',' 8.53','12.03'),
                    Std = c('7.27','6.91','7.21','6.86','6.03','7.19'),
                    COV = c('0.879','1.016','0.986','0.813','0.707','0.598'),
                    Min = c('-1.568', '-1.844',' 0.221','-0.676',' 0.315','-0.616'),
                    Q1 = c('1.172', '0.497','0.854','1.227','5.344','9.191'),
                    Median = c(' 8.66',' 6.28',' 3.27',' 7.47',' 9.81','12.07'),
                    Q3 = c('14.6', '12.7','14.9','15.4','11.1','15.9'),
                    Max = c('19.6','20.6','16.6','19.2','19.3','23.1'),
                    VolumnRaw = c("12", "32", "11", "28", " 9", " 8"),
                    Impact = c("10.54", "32.51", "10.84", "22.76", " 6.36", " 4.78"),
                    stringsAsFactors = FALSE)
  
  expected <- list(df1,df2)
  names(expected) <- c("P value for each pair of groups", 
                       "Basic statistics of each group")
  
  
  # Drop row names, so the two dfs can match w/o specifying row names explicitly
  rownames(dfRes[[1]]) <- c(); rownames(dfRes[[2]]) <- c()
  rownames(expected[[1]]) <- c(); rownames(expected[[2]]) <- c()
  
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
  
  dfRes[[1]] <- data.frame(lapply(dfRes[[1]], as.character), 
                           stringsAsFactors = FALSE)
  dfRes[[2]] <- data.frame(lapply(dfRes[[2]], as.character), 
                           stringsAsFactors = FALSE)
  
  df1 <- data.frame(Measure = c('LOS','LOS','LOS','LOS','LOS','LOS',
                                'LOS','LOS','LOS','LOS','LOS','LOS',
                                'LOS','LOS','LOS'),
                    Groups = c("F.2012/03-M.2012/01","F.2012/02-M.2012/01","M.2012/03-M.2012/01",
                               "M.2012/02-M.2012/01","F.2012/03-F.2012/01","F.2012/02-F.2012/01",
                               "F.2012/01-M.2012/01","F.2012/03-M.2012/02","F.2012/03-M.2012/03",
                               "M.2012/03-F.2012/01","M.2012/02-F.2012/01","F.2012/02-M.2012/02",
                               "F.2012/02-M.2012/03","F.2012/03-F.2012/02","M.2012/03-M.2012/02"), 
                    p_value = c("0.690788","0.760393","0.923330","0.923330","0.935469",
                                "0.967513","0.987223","0.994585","0.994585","0.998789",
                                "0.998789","0.998831","0.998831","0.999984","1.000000"),
                    stringsAsFactors = FALSE)
  
  df2 <- data.frame(measure = c('LOS','LOS','LOS','LOS','LOS','LOS'),
                         group = c('F.2012/01','M.2012/02','M.2012/01','F.2012/03','M.2012/03','F.2012/02'),
                         Mean = c('3.40','4.10','2.10','5.00','4.10','4.75'),
                         Std = c('1.510','2.476','0.424','2.708','2.265','2.500'),
                         COV = c('0.444','0.604','0.202','0.542','0.552','0.526'),
                         Min = c('2.0', '1.3','1.8','3.0','2.0','2.0'),
                         Q1 = c('2.60', '3.15','1.95','3.75','2.90','3.50'),
                         Median = c('3.2','5.0','2.1','4.0','3.8','4.5'),
                         Q3 = c('4.10', '5.50','2.25','5.25','5.15','5.75'),
                         Max = c('5.0','6.0','2.4','9.0','6.5','8.0'),
                         VolumnRaw = c("3", "3", "2", "4", "3", "4"),
                         Impact = c("1.332", "1.812", "0.404", "2.166", "1.657", "2.105"),
                         stringsAsFactors = FALSE)
  
  
  expected <- list(df1,df2)
  names(expected) <- c("P value for each pair of groups", 
                       "Basic statistics of each group")
  
  
  # Drop row names, so the two dfs can match w/o specifying row names explicitly
  rownames(dfRes[[1]]) <- c(); rownames(dfRes[[2]]) <- c()
  rownames(expected[[1]]) <- c(); rownames(expected[[2]]) <- c()
  
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
            
            dfRes[[1]] <- data.frame(lapply(dfRes[[1]], as.character), 
                                     stringsAsFactors = FALSE)
            dfRes[[2]] <- data.frame(lapply(dfRes[[2]], as.character), 
                                     stringsAsFactors = FALSE)
            
            df1 <- data.frame(Measure = c('LOS','BP'),
                                Groups = c('F-M','F-M'), 
                              p_value = c("0.234984","0.804558"),
                              stringsAsFactors = FALSE)
            
            df2 <- data.frame(measure = c('LOS','LOS','BP','BP'),
                                   group = c('F','M','F','M'),
                                   Mean = c('  5.3','  2.9','121.5','109.7'),
                                   Std = c(' 2.57',' 1.90','44.98','34.93'),
                                   COV = c('0.486','0.655','0.370','0.319'),
                                   Min = c(' 3.2', ' 1.3','58.0','89.0'),
                                   Q1 = c('  3.80', '  1.85','106.75',' 89.50'),
                                   Median = c('  4.5','  2.4','134.0',' 90.0'),
                                   Q3 = c('  6.0', '  3.7','148.8','120.0'),
                                   Max = c('  9','  5','160','150'),
                                   VolumnRaw = c("4", "3", "4", "3"),
                                   Impact = c("1.943", "1.966", "1.481", "0.956"),
                                   stringsAsFactors = FALSE)
            
            expected <- list(df1,df2)
            names(expected) <- c("P value for each pair of groups", 
                                 "Basic statistics of each group")
            
            
            # Drop row names, so the two dfs can match w/o specifying row names explicitly
            rownames(dfRes[[1]]) <- c(); rownames(dfRes[[2]]) <- c()
            rownames(expected[[1]]) <- c(); rownames(expected[[2]]) <- c()
            
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
  
  dfRes[[1]] <- data.frame(lapply(dfRes[[1]], as.character), 
                           stringsAsFactors = FALSE)
  dfRes[[2]] <- data.frame(lapply(dfRes[[2]], as.character), 
                           stringsAsFactors = FALSE)
  
  df1 <- data.frame(Measure = c('LOS','LOS','LOS','LOS','LOS','LOS',
                                'LOS','LOS','LOS','LOS','LOS','LOS',
                                'LOS','LOS','LOS','BP','BP','BP',
                                'BP','BP','BP','BP','BP','BP',
                                'BP','BP','BP','BP','BP','BP'),
                    Groups = c("F.2012/03-M.2012/01","F.2012/02-M.2012/01","M.2012/03-M.2012/01",
                               "M.2012/02-M.2012/01","F.2012/03-F.2012/01","F.2012/02-F.2012/01",
                               "F.2012/01-M.2012/01","F.2012/03-M.2012/02","F.2012/03-M.2012/03",
                               "M.2012/03-F.2012/01","M.2012/02-F.2012/01","F.2012/02-M.2012/02",
                               "F.2012/02-M.2012/03","F.2012/03-F.2012/02","M.2012/03-M.2012/02",
                               "F.2012/01-F.2012/03","M.2012/01-F.2012/03","F.2012/02-F.2012/03",
                               "F.2012/01-M.2012/02","F.2012/01-M.2012/03","M.2012/01-M.2012/02",
                               "M.2012/01-M.2012/03","F.2012/02-M.2012/02","F.2012/02-M.2012/03",
                               "M.2012/03-F.2012/03","M.2012/02-F.2012/03","F.2012/01-F.2012/02",
                               "M.2012/01-F.2012/02","F.2012/01-M.2012/01","M.2012/03-M.2012/02"), 
                    p_value = c("0.690788","0.760393","0.923330","0.923330","0.935469",
                                "0.967513","0.987223","0.994585","0.994585","0.998789",
                                "0.998789","0.998831","0.998831","0.999984","1.000000",
                                '0.963084','0.964873','0.987256','0.995431','0.995798',
                                '0.995798','0.996142','0.999513','0.999581','0.999730',
                                '0.999773','0.999909','0.999927','1.000000','1.000000'),
                    stringsAsFactors = FALSE)
  
  df2 <- data.frame(measure = c('LOS','LOS','LOS','LOS','LOS','LOS',
                                     'BP','BP','BP','BP','BP','BP'),
                         group = c('F.2012/01','M.2012/02','M.2012/01','F.2012/03','M.2012/03','F.2012/02',
                                   'F.2012/01','M.2012/02','M.2012/01','F.2012/03','M.2012/03','F.2012/02'),
                         Mean = c('  3.40','  4.10','  2.10','  5.00','  4.10','  4.75',
                                  '131.00','112.33','131.50','103.00','112.67','123.25'),
                         Std = c(' 1.510',' 2.476',' 0.424',' 2.708',' 2.265',' 2.500',
                                 '12.166','32.929','58.690','64.052','67.122','27.464'),
                         COV = c( "0.4441","0.6039","0.2020","0.5416","0.5524",
                                  "0.5263","0.0929","0.2931","0.4463","0.6219",
                                  "0.5958","0.2228"),
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
                         VolumnRaw = c("3", "3", "2", "4", "3", "4", "3", "3", "2", "4", "3", "4"),
                         Impact = c("1.332", "1.812", "0.404", "2.166", "1.657", "2.105", "0.279",
                                    "0.879", "0.893", "2.487", "1.787", "0.891"),
                         stringsAsFactors = FALSE)
  
  expected <- list(df1,df2)
  names(expected) <- c("P value for each pair of groups", 
                       "Basic statistics of each group")
  
  
  # Drop row names, so the two dfs can match w/o specifying row names explicitly
  rownames(dfRes[[1]]) <- c(); rownames(dfRes[[2]]) <- c()
  rownames(expected[[1]]) <- c(); rownames(expected[[2]]) <- c()
  
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


test_that("One cat column, one measure col, and no date col, no NA gives correct 
          boxplot", {
            set.seed((35))
            treatment <- c(rep("A", 20) , rep("B", 20) , rep("C", 20), rep("D", 20) ,  
                           rep("E", 20))
            value <- c( sample(2:5, 20 , replace = TRUE) , 
                        sample(6:10, 20 , replace = TRUE), 
                        sample(1:7, 20 , replace = TRUE), 
                        sample(3:10, 20 , replace = TRUE) , 
                        sample(10:20, 20 , replace = TRUE))
            df <- data.frame(treatment,value)
            
            dfRes <- variationAcrossGroups(df = df, 
                                           categoricalCols = "treatment", 
                                           measureColumn = "value",
                                           printTable = FALSE,
                                           boxplotStats = TRUE)
            
            expected1 <- list()
            expected1$stats <- matrix(c(2,3,4,5,5,6.0,7.0,7.5,9.5,10.0,2,4,5,6,7,
                                       3,5,6,9,10,
                                       10.0,13.0,15.0,17.5,19.0), nrow = 5, ncol = 5)
            attributes(expected1$stats)$class <- "integer"
            names(attributes(expected1$stats)$class) <- "A"
            
            expected1$n <- c(20,20,20,20,20)
            expected1$conf <- matrix(c(3.293403,4.706597,6.616753,8.383247,4.293403,
                                      5.706597,4.586805,
                                      7.413195,13.41016,16.58984), nrow = 2, ncol = 5)
            
            
            expected1$out <- numeric(0)
            expected1$group <- numeric(0)
            expected1$names <- c("A","B","C","D","E")
            
            expected <- list(expected1)
            names(expected) <- "value"
            
            testthat::expect_equal(dfRes,expected,tolerance = 1e-6)
          })


test_that("One cat column, two measure col, and no date col, no NA gives correct 
          boxplot", {
            set.seed((35))
            treatment <- c(rep("A", 20) , rep("B", 20) , rep("C", 20), rep("D", 20) ,  
                           rep("E", 20))
            value1 <- c(sample(2:5, 20 , replace = TRUE) , 
                        sample(6:10, 20 , replace = TRUE), 
                        sample(1:7, 20 , replace = TRUE), 
                        sample(3:10, 20 , replace = TRUE) , 
                        sample(10:20, 20 , replace = TRUE))
            value2 <- c(sample(2:10, 20 , replace = TRUE) , 
                        sample(8:20, 20 , replace = TRUE), 
                        sample(3:9, 20 , replace = TRUE), 
                        sample(1:6, 20 , replace = TRUE) , 
                        sample(20:40, 20 , replace = TRUE))
            
            df <- data.frame(treatment,value1,value2)
            
            dfRes <- variationAcrossGroups(df = df, 
                                           categoricalCols = "treatment", 
                                           measureColumn = c("value1","value2"),
                                           printTable = FALSE,
                                           boxplotStats = TRUE)
            
            expected1 <- list()
            expected1$stats <- matrix(c(2,3,4,5,5,6.0,7.0,7.5,9.5,10.0,2,4,5,6,7,
                                        3,5,6,9,10,
                                        10.0,13.0,15.0,17.5,19.0), nrow = 5, ncol = 5)
            attributes(expected1$stats)$class <- "integer"
            names(attributes(expected1$stats)$class) <- "A"
            
            expected1$n <- c(20,20,20,20,20)
            expected1$conf <- matrix(c(3.293403,4.706597,6.616753,8.383247,4.293403,
                                       5.706597,4.586805,
                                       7.413195,13.41016,16.58984), nrow = 2, ncol = 5)
            
            
            expected1$out <- numeric(0)
            expected1$group <- numeric(0)
            expected1$names <- c("A","B","C","D","E")
            
            expected2 <- list()
            expected2$stats <- matrix(c(2.0,3.0,4.0,7.5,10.0,9.0,11.5,13.0,17.0,20.0,3.0,4.0,6.0,7.5,9.0,
                                        1,3,4,5,6,
                                        20.0,27.0,30.0,36.5,40.0), nrow = 5, ncol = 5)
            attributes(expected2$stats)$class <- "integer"
            names(attributes(expected2$stats)$class) <- "A"
            
            expected2$n <- c(20,20,20,20,20)
            expected2$conf <- matrix(c(2.410156,5.589844,11.05686,14.94314,4.763454,
                                       7.236546,3.293403,
                                       4.706597,26.64366,33.35634), nrow = 2, ncol = 5)
            
            
            expected2$out <- numeric(0)
            expected2$group <- numeric(0)
            expected2$names <- c("A","B","C","D","E")
            
            expected <- list(expected1,expected2)
            names(expected) <- c("value1","value2")
            
            testthat::expect_equal(dfRes,expected,tolerance = 1e-6)
          })

