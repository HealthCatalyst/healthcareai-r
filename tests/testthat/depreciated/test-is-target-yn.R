context('Checking that isTargetYN is working')

test_that("For a vector of all 0's and 1's, function returns expected output,
           TRUE", {
            
  dat <- data.frame(a = c(0,1,1,0,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0))
  
  actualOut <- isTargetYN(dat[, 1])

  expected <- FALSE

  expect_equal(actualOut, expected)

})

test_that("For a vector of all numbers, function returns expected output,
           FALSE", {
               
  dat <- data.frame(a = c(3, 4, 5, 6, 7, 8, 9))
                 
  actualOut <- isTargetYN(dat[, 1])
                   
  expected <- FALSE
                     
  expect_equal(actualOut, expected)
                     
})

test_that("For a vector of Y's, y's, N's, and n's, function returns expected
           output, FALSE", {
               
  dat <- data.frame(a = c('y', 'n', 'Y', 'y', 'n', 'Y', 'Y', 'n', 'N', 'Y'))
                 
  actualOut <- isTargetYN(dat[, 1])
                   
  expected <- FALSE
                     
  expect_equal(actualOut, expected)
                     
})

test_that("For a simple vector of Y's, N's, and NA's, function returns expected
           output, TRUE", {
               
  dat <- data.frame(a = c('Y', 'N', NA, 'N', 'Y', 'Y', NA, 'N', 'N', 'Y',
                          NA, NA))
                 
  actualOut <- isTargetYN(dat[, 1])
                   
  expected <- TRUE
                     
  expect_equal(actualOut, expected)
                     
})

  
test_that("For a simple vector of Y's and N's, function returns expected output,
           TRUE", {
               
  dat <- data.frame(a = c('Y', 'N', 'Y', 'N', 'Y', 'Y', 'Y', 'N', 'N', 'Y'))
                 
  actualOut <- isTargetYN(dat[, 1])
                   
  expected <- TRUE
                     
  expect_equal(actualOut, expected)
                     
})

test_that("For a given parameter list, setconfigs() throws error when checking
          isTargetYN()",{
  
  csvfile <- system.file("extdata", "HCRDiabetesClinical.csv", 
                         package = "healthcareai")
            
  df <- read.csv(file = csvfile, header = TRUE, na.strings =  
                   c('NULL', 'NA', ""))
  #change so ThirtyDayReadmitFLG is y/N instead of Y/N
  df$ThirtyDayReadmitFLG <- ifelse(df$ThirtyDayReadmitFLG == "Y", "y", "N") 
  
  set.seed(43)
  p <- SupervisedModelDevelopmentParams$new()
  p$df = df
  p$type = 'classification'
  p$grainCol = 'PatientEncounterID'
  p$impute = TRUE
  p$predictedCol = "ThirtyDayReadmitFLG"
  p$debug = FALSE
  p$cores = 1
  
  expect_error(lmm <- LinearMixedModelDevelopment$new(p), 
               'predictedCol must be Y/N. IIF function in sql may help')

})