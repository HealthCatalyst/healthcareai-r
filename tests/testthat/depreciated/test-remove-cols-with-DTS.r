context('Checking that removeColsWithDTSSuffix is working')

test_that("For a dataframe with one DTS column, function returns expected 
          output", {
            
  df <- data.frame(testDTS = c(1,2,3),
                   b = c('Y','N',NA),
                   c = c(NA,'Y','N'))
    
  df <- removeColsWithDTSSuffix(df)
    
  expecteddf <- data.frame(b = c('Y','N',NA),
                           c = c(NA,'Y','N'))
    
  expect_identical(df, expecteddf)
})

test_that("For a dataframe with multiple DTS columns, function returns expected
          output", {
  
  df2 <- data.frame(testDTS = c(1,2,3),
                    b = c('Y','N',NA),
                    test2DTS = c(NA,'Y','N'))
  
  df2 <- removeColsWithDTSSuffix(df2)
  
  expecteddf2 <- data.frame(b = c('Y','N',NA))
  
  expect_identical(df2, expecteddf2)
})

test_that("For a dataframe with no DTS columns, function returns expected 
          output", {
  
  df3 <- data.frame(a = c(1,2,3),
                    b = c('Y','N',NA),
                    c = c(NA,'Y','N'))

  df3 <- removeColsWithDTSSuffix(df3)
  
  expecteddf3 <- data.frame(a = c(1,2,3),
                            b = c('Y','N',NA),
                            c=c(NA,'Y','N'))
  
  expect_identical(df3, expecteddf3)
})


