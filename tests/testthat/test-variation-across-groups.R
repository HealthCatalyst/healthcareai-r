context("Checking variationAcrossGroups")

test_that("One cat column, one measure col, and all defaults gives single data frame
          with correct column types, mean difference, and p-value.", {

            df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                             LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
            
            dfRes <- variationAcrossGroups(df = df,
                                           categoricalCols = "Gender",
                                           measureColumn = "LOS")
            
            testthat::expect_true(is.data.frame(dfRes))
            testthat::expect_equal(dfRes$`Mean Difference`, 
                                   abs(mean(df$LOS[df$Gender == "M"], na.rm = TRUE) - 
                                         mean(df$LOS[df$Gender == "F"], na.rm = TRUE))
            )
            testthat::expect_equal(dfRes$`Adjusted p-value`, 0.235)
})

test_that("One cat column, one measure col, and no date col and NA in cat col 
          give correct df", {
            df <- data.frame(Gender = c('F','M','M','M','M','F',NA,'F'),
                             LOS = c(3.2,NA,5,1.3,2.4,4,9,5))
            
            dfRes <- variationAcrossGroups(df = df,
                                           categoricalCols = "Gender",
                                           measureColumn = "LOS")
            
            df1 <- data.frame(Groups = 'F-M', 
                              `Mean Difference` = 1.167,
                              `Adjusted p-value` = 0.391063, 
                              check.names = FALSE)
            
            testthat::expect_equal(dfRes, df1, tolerance = .001)
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
  
  df1 <- data.frame(Groups = c("B.Male-C.Male","B.Male-A.Female","B.Male-C.Female",  
                               "B.Male-B.Female","B.Male-A.Male","C.Female-C.Male",  
                               "A.Male-C.Male","B.Female-C.Male","C.Female-A.Female",
                               "A.Male-A.Female","B.Female-A.Female","A.Female-C.Male",  
                               "A.Male-B.Female","C.Female-B.Female","A.Male-C.Female"), 
                    `Mean Difference` = 
                      c(5.232, 4.716, 3.589, 3.756, 3.5, 1.643, 1.733, 1.476, 1.127, 
                        1.216, 0.96, 0.516, 0.256, 0.167, 0.089),
                    `Adjusted p-value` = 
                      as.numeric(c("0.401138","0.686254","0.787918","0.841090","0.902915",
                                   "0.941093","0.985462","0.988487","0.997409","0.998782",
                                   "0.999447","0.999937","0.999999","1.000000","1.000000")),
                    check.names = FALSE)
  
  
  testthat::expect_equal(dfRes, df1, tolerance = 1e-2)
  
})

test_that("One cat col, one measure col, and one date col give correct df", {
  df <- data.frame(Gender = c('F','F','M','M','M','M','F','F'),
                   LOS = c(3, 4.3, 5.3, 4, 9, 8, 2, 3.8),
                   StartDTS = c('2012-01-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-01-01 10:04:23', '2012-01-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-01 10:04:23',
                                '2012-02-01 10:04:23', '2012-02-01 10:04:23'))
  
  dfRes <- variationAcrossGroups(df = df,
                                 categoricalCols = "Gender",
                                 measureColumn = "LOS",
                                 dateCol = 'StartDTS')
  
  df1 <- data.frame(Groups = c('M.2012/02-F.2012/02',
                               'M.2012/02-F.2012/01',
                               'M.2012/02-M.2012/01',
                               'M.2012/01-F.2012/02',
                               'M.2012/01-F.2012/01',
                               'F.2012/01-F.2012/02'), 
                    `Mean Difference` = c(5.60, 4.85, 3.85 ,1.75 ,1.00, 0.75),
                    `Adjusted p-value` = as.numeric(c("0.016","0.026","0.055","0.392","0.746", "0.865")),
                    check.names = FALSE)

  testthat::expect_equal(dfRes, df1, tolerance = 1e-3)
})

test_that("Two measure columns throws error", {
  df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   BP = c(123,129,89,150,90,58,160,145))
  
  testthat::expect_error(variationAcrossGroups(df = df,
                                               categoricalCols = "Gender",
                                               measureColumn = c("LOS", "BP")), 
                         regexp = "can only specify one measure column")
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

test_that("Various return specifications are of appropriate length and type", {
    df <- data.frame(Gender = c('F','M','M','M','M','F','F','F'),
                   LOS = c(3.2,NA,5,1.3,2.4,4,9,5),
                   Age = c(23,45,63,42,78,32,15,65))
    
    defaultOut <- variationAcrossGroups(df, "Gender", "LOS")
    
    testthat::expect_true(is.data.frame(defaultOut))
    testthat::expect_equal(sapply(defaultOut, is.numeric), 
                           c(FALSE, TRUE, TRUE),
                           check.attributes = FALSE)
    
    withGroups <- variationAcrossGroups(df, "Gender", "LOS", returnGroupStats = TRUE)
    
    testthat::expect_equal(length(withGroups), 2)
    testthat::expect_false(is.data.frame(withGroups))
    testthat::expect_true(is.data.frame(withGroups[[2]]))
    testthat::expect_named(withGroups)
    testthat::expect_equal(sapply(withGroups[[2]], is.numeric), 
                           c(rep(FALSE, 2), rep(TRUE, 10)),
                           check.attributes = FALSE)
  
})

