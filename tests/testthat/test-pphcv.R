context("Checking pre-processing of high-cardinality variables")

set.seed(10)
dat <- data.frame(Ycontinuous = sample(5:42, size = 14, replace = TRUE),
                  Ycategorical = sample(c("Y", "N"), size = 14, 
                                        replace = TRUE, prob = c(0.45, 0.55)),
                  YCat01 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0),
                  hcX = sample(letters[1:5], size = 14, replace = TRUE),
                  hcX2 = sample(letters[6:10], size = 14, replace = TRUE),
                  hcX3 = sample(letters[11:15], size = 14, replace = TRUE),
                  Ymulticlass = sample(c("blue", "green", "red"), size = 14, 
                                       replace = TRUE))

set.seed(12)
dat2 <- data.frame(hcX = sample(letters[1:5], size = 14, replace = TRUE),
                   hcX2 = sample(letters[6:10], size = 14, replace = TRUE),
                   hcX3 = sample(letters[11:15], size = 14, replace = TRUE))

# ------------------------------------------------------------------------------
test_that("Continuous target, one high_cardinality variable, and no 
          specified m_tuning_param, pphcv returns correct, new feature", {

  b <- pphcv(df = dat, target = "Ycontinuous", high_card = "hcX", 
             type = "continuous")
  
  expect_equal(b$newdf$hcX_new[1], 22.05357, tolerance = 0.001)
  expect_equal(b$newdf$hcX_new[12], 20.55357, tolerance = 0.001)
})

test_that("For a binary target, one high-cardinality variable, one specified
          positilve class and no specified m_tuning_param, pphcv returns 
          correct, new feature", {
            
  b <- pphcv(df = dat, target = "Ycategorical", high_card = "hcX", 
             pos_class = "Y", type = "binary")
            
  expect_equal(b$newdf$hcX_new[5], 0.1428571, tolerance = 0.001)
  expect_equal(b$newdf$hcX_new[14], 0.3571429, tolerance = 0.001)
})

test_that("For a binary target coded as numeric(0, 1) and one high-cardinality 
          variable and specifying the pos_class with no specified 
          m_tuning_param, pphcv returns correct, new feature", {
            
 b <- pphcv(df = dat, target = "YCat01", high_card = "hcX", pos_class = 1, 
            type = "binary")
            
 expect_equal(b$newdf$hcX_new[5], 0.1428571, tolerance = 0.001)
 expect_equal(b$newdf$hcX_new[14], 0.3571429, tolerance = 0.001)
})

test_that("For a binary target coded as numeric(0,1), one 
          high-cardinality variable, type = 'continuous', no specified 
          pos_class, and no specified m_tuning_param, pphcv returns correct, new
          feature", {
            
 b <- pphcv(df = dat, target = "YCat01", high_card = "hcX", type = "continuous")
            
 expect_equal(b$newdf$hcX_new[5], 0.1428571, tolerance = 0.001)
 expect_equal(b$newdf$hcX_new[14], 0.3571429, tolerance = 0.001)
})

test_that("For a multiclass target , one high-cardinality variable, and 
          type = 'multiclass', no specified multi_base, and no specified 
          m_tuning_param, pphcv returns correct, new features and correct number
          of new features", {
            
 b <- pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
            type = "multiclass")
            
 expect_equal(b$newdf$hcX_green_new[7], 0.875, tolerance = 0.001)
 expect_equal(b$newdf$hcX_green_new[10], 0.375, tolerance = 0.001)
 expect_equal(b$newdf$hcX_red_new[7], 0.03571429, tolerance = 0.001)
 expect_equal(b$newdf$hcX_red_new[8], 0.5357143, tolerance = 0.001)
 expect_equal((length(dat) + length(levels(dat$Ymulticlass)) - 1), 
               length(b$newdf), tolerance = 0)
})

test_that("For a multiclass target , one high-cardinality variable, and 
          type = 'multiclass', multi_base = 'green', and no specified 
          m_tuning_param, pphcv returns correct, new features and correct number
          of new features", {
            
 b <- pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
                       type = "multiclass", multi_base = "green")
            
 expect_equal(b$newdf$hcX_blue_new[7], 0.08928571, tolerance = 0.001)
 expect_equal(b$newdf$hcX_blue_new[10], 0.5892857, tolerance = 0.001)
 expect_equal(b$newdf$hcX_red_new[7], 0.03571429, tolerance = 0.001)
 expect_equal(b$newdf$hcX_red_new[8], 0.5357143, tolerance = 0.001)
 expect_equal((length(dat) + length(levels(dat$Ymulticlass)) - 1), 
                         length(b$newdf), tolerance = 0)
 expect_false("green" %in% b$mappings$multi_levels)
})

# Test that pphcv works with vectors of high-C variables------------------------
test_that("For a binary target, a vector of high-cardinality variables, 
          type = 'binary', pos_class = 'Y', pphcv returns correct new features 
          and correct number of new features", {

 vec <- c("hcX", "hcX2", "hcX3")
 b <- pphcv(df = dat, target = "Ycategorical", high_card = vec, 
            type = "binary", pos_class = "Y")
 expect_equal(b$newdf$hcX_new[4], 0.1428571, tolerance = 0.001)
 expect_equal(b$newdf$hcX2_new[4], 0.4047619, tolerance = 0.001)
 expect_equal(b$newdf$hcX3_new[4], 0.3469388, tolerance = 0.001)
 expect_equal(length(dat) + length(vec), length(b$newdf), tolerance = 0)
})

test_that("For a continuous target, a vector of high-cardinality variables, and 
          type = 'continuous', pphcv returns correct new features and correct 
          number of new features", {
            
 vec <- c("hcX", "hcX2", "hcX3")
 b <- pphcv(df = dat, target = "Ycontinuous", high_card = vec, 
            type = "continuous")
 
 expect_equal(b$newdf$hcX_new[10], 20.55357, tolerance = 0.001)
 expect_equal(b$newdf$hcX2_new[10], 18.7381, tolerance = 0.001)
 expect_equal(b$newdf$hcX3_new[10], 20.60714, tolerance = 0.001)
 expect_equal(length(dat) + length(vec), length(b$newdf), tolerance = 0)
})

test_that("For a multiclass target, a vector of high-cardinality variables,
          type = 'multiclass', no specified multi_base, pphcv returns correct 
          new features and correct number of new features", {
 
 vec <- c("hcX", "hcX2", "hcX3")
 b <- pphcv(df = dat, target = "Ymulticlass", high_card = vec, 
            type = "multiclass")
 
 expect_equal(b$newdf$hcX_green_new[3], 0.875, tolerance = 0.001)
 expect_equal(b$newdf$hcX_red_new[3], 0.03571429, tolerance = 0.001)
 expect_equal(b$newdf$hcX2_green_new[3], 0.375, tolerance = 0.001)
 expect_equal(b$newdf$hcX2_red_new[3], 0.2857143, tolerance = 0.001)
 expect_equal(b$newdf$hcX3_green_new[3], 0.625, tolerance = 0.001)
 expect_equal(b$newdf$hcX3_red_new[3], 0.2857143, tolerance = 0.001)
 expect_equal((length(dat) + (length(vec) * length(levels(dat$Ymulticlass)) 
                              - length(vec))), 13, tolerance = 0)
})

# Test deployment of mappings---------------------------------------------------

## Continuous target and one high_cardinality variable
test_that("For a continuous target, one high-cardinality variable, mapping to 
          new data returns correct probabilities for each level of the 
          high-cardinality variable", {

 f <- pphcv(df = dat, target = "Ycontinuous", high_card = "hcX", 
            type = "continuous", remove_high_card = FALSE)

 f1 <- pphcv(df = dat2, high_card = "hcX", mapping = f$mappings, 
             type = "continuous")
 
 expect_equal(f$newdf$hcX_new[f$newdf$hcX == "a"][1], 
              f1$newdf$hcX_new[f1$newdf$hcX == "a"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX_new[f$newdf$hcX == "e"][1], 
              f1$newdf$hcX_new[f1$newdf$hcX == "e"][1], tolerance = 0.001)
 
 # for levels that don't exist in the new data, test that they don't show up
 expect_true(is.na(f1$newdf$hcX_new[f1$newdf$hcX == "c"][1]))
})

test_that("For a binary target, one high-cardinality variable, mapping to new 
          data returns correct probabilities for each level of the 
          high-cardinality variable", {
            
 f <- pphcv(df = dat, target = "Ycategorical", high_card = "hcX", 
            pos_class = "Y", type = "binary")
 
 f1 <- pphcv(df = dat2, high_card = "hcX", mapping = f$mappings, 
             type = "binary")
            
 expect_equal(f$newdf$hcX_new[f$newdf$hcX == "a"][1], 
              f1$newdf$hcX_new[f1$newdf$hcX == "a"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX_new[f$newdf$hcX == "d"][1], 
              f1$newdf$hcX_new[f1$newdf$hcX == "d"][1], tolerance = 0.001)
 
 # for levels that don't exist in the new data, test that they don't show up
 expect_true(is.na(f1$newdf$hcX_new[f1$newdf$hcX == "c"][1]))
})

test_that("For a multi-class target, one high-cardinality variable, mapping to 
          new data returns correct probabilities for each level of the 
          high-cardinality variable and each non-base class", {
            
 f <- pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
            type = "multiclass")
 f1 <- pphcv(df = dat2, high_card = "hcX", mapping = f$mappings, 
             type = "multiclass")
            
 expect_equal(f$newdf$hcX_green_new[f$newdf$hcX == "b"][1], 
              f1$newdf$hcX_green_new[f1$newdf$hcX == "b"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX_red_new[f$newdf$hcX == "d"][1], 
              f1$newdf$hcX_red_new[f1$newdf$hcX == "d"][1], tolerance = 0.001)
 
 # for levels that don't exist in the new data, test that they don't show up
 expect_true(is.na(f1$newdf$hcX_green_new[f1$newdf$hcX == "c"][1]))
})

test_that("For a continuous target, a vector of high-cardinality variables, 
          mapping to new data returns correct probabilities for each level of 
          the high-cardinality variable", {
 
 vec <- c("hcX", "hcX2", "hcX3")       
 f <- pphcv(df = dat, target = "Ycontinuous", high_card = vec, 
            type = "continuous", remove_high_card = FALSE)
            
 f1 <- pphcv(df = dat2, high_card = vec, mapping = f$mappings, 
             type = "continuous")
            
 expect_equal(f$newdf$hcX_new[f$newdf$hcX == "a"][1], 
              f1$newdf$hcX_new[f1$newdf$hcX == "a"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX2_new[f$newdf$hcX2 == "b"][1], 
              f1$newdf$hcX2_new[f1$newdf$hcX2 == "b"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX3_new[f$newdf$hcX3 == "d"][1], 
              f1$newdf$hcX3_new[f1$newdf$hcX3 == "d"][1], tolerance = 0.001)

 # for levels that don't exist in the new data, test that they don't show up
 expect_true(is.na(f1$newdf$hcX3_new[f1$newdf$hcX3 == "c"][1]))
})

test_that("For a binary target, a vector of high-cardinality variables, mapping 
          to new data returns correct probabilities for each level of the 
          high-cardinality variable", {

 vec <- c("hcX", "hcX2", "hcX3")            
 f <- pphcv(df = dat, target = "Ycategorical", high_card = vec, pos_class = "Y",
            type = "binary")
            
 f1 <- pphcv(df = dat2, high_card = vec, mapping = f$mappings, type = "binary")
            
 expect_equal(f$newdf$hcX_new[f$newdf$hcX == "a"][1], 
              f1$newdf$hcX_new[f1$newdf$hcX == "a"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX2_new[f$newdf$hcX2 == "b"][1], 
              f1$newdf$hcX2_new[f1$newdf$hcX2 == "b"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX3_new[f$newdf$hcX3 == "d"][1], 
              f1$newdf$hcX3_new[f1$newdf$hcX3 == "d"][1], tolerance = 0.001)
            
 # for levels that don't exist in the new data, test that they don't show up
 expect_true(is.na(f1$newdf$hcX2_new[f1$newdf$hcX2 == "c"][1]))
})

test_that("For a multi-class target, a vector of high-cardinality variables, 
          mapping to new data returns correct probabilities for each level of 
          the high-cardinality variable and each non-base class", {
            
 vec <- c("hcX", "hcX2", "hcX3")             
 f <- pphcv(df = dat, target = "Ymulticlass", high_card = vec, 
            type = "multiclass")
 f1 <- pphcv(df = dat2, high_card = vec, mapping = f$mappings, 
             type = "multiclass")
            
 expect_equal(f$newdf$hcX_green_new[f$newdf$hcX == "a"][1], 
              f1$newdf$hcX_green_new[f1$newdf$hcX == "a"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX2_green_new[f$newdf$hcX2 == "b"][1], 
              f1$newdf$hcX2_green_new[f1$newdf$hcX2 == "b"][1], 
              tolerance = 0.001)
 expect_equal(f$newdf$hcX_red_new[f$newdf$hcX == "d"][1], 
              f1$newdf$hcX_red_new[f1$newdf$hcX == "d"][1], tolerance = 0.001)
 expect_equal(f$newdf$hcX2_red_new[f$newdf$hcX2 == "e"][1], 
              f1$newdf$hcX2_red_new[f1$newdf$hcX2 == "e"][1], tolerance = 0.001)
            
 # for levels that don't exist in the new data, test that they don't show up
 expect_true(is.na(f1$newdf$hcX3_green_new[f1$newdf$hcX3 == "c"][1]))
})

test_that("When specifying remove_high_card = TRUE, correct number of columns
          is returned", {
 
 high_card = "hcX"
 b <- pphcv(df = dat, target = "Ymulticlass", high_card = high_card, 
            type = "multiclass", remove_high_card = TRUE)
 
 expect_equal((length(dat) + length(levels(dat$Ymulticlass)) - 1) 
              - length(high_card), length(b$newdf), tolerance = 0)
})

test_that("when specifying a multi_base for type = 'multiclass', correct base
          class is used", {

b <- pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
                       type = "multiclass", multi_base = "red")

expect_equal(length(grep("red", names(b$newdf), value = TRUE)), 0, 
             tolerance = 0)
})

test_that("when specifying an m_tuning_param, pphcv returns correct new 
          feature", {
  
  b <- pphcv(df = dat, target = "Ycategorical", high_card = "hcX", 
             type = "binary", m_tuning_param = 10, pos_class = "Y")
  
  expect_equal(b$newdf$hcX_new[7], 0.4835165, tolerance = 0.001)
  expect_equal(b$newdf$hcX_new[14], 0.4065934, tolerance = 0.001)
})

# Error Checking ---------------------------------------------------------------
test_that("Correct error is thrown if target is not in the dataframe", {
  
  expect_error(pphcv(df = dat, target = "SomeRandom", high_card = "hCX", 
                     type = "binary", pos_class = "Y"),
               "Your specified target/outcome is not in your given dataframe")
})

test_that("pos_class not in target throws correct error",{
  expect_error(pphcv(df = dat, target = "Ycategorical", high_card = "hCX",
                     type = "binary", pos_class = "M"),
               "Your specified pos_class is not in your given target")
  expect_error(pphcv(df = dat, target = "Ycategorical", high_card = "hCX",
                     type = "binary", pos_class = 5),
               "Your specified pos_class is not in your given target")
  expect_error(pphcv(df = dat, target = "YCat01", high_card = "hCX",
                     type = "binary", pos_class = "M"),
               "Your specified pos_class is not in your given target")
  expect_error(pphcv(df = dat, target = "YCat01", high_card = "hCX",
                     type = "binary", pos_class = 5),
               "Your specified pos_class is not in your given target")
  
})

test_that("For type = binary, if target > 2 classes/levels, throw error", {
  dat <- data.frame(Ycategorical = c('Y', "N", "N", "Y", "N", "Y", "N", "Y", "Y", 
                                     "Y", "M", "M", "N", "Y"),
                    YCat01 = c(0, 0, 3, 0, 3, 1, 1, 1, 1, 0, 0, 1, 1, 0),
                    hcX = sample(letters[1:5], size = 14, replace = TRUE))
  expect_error(pphcv(df = dat, target = "Ycategorical", high_card = "hcX",
                     type = "binary", pos_class = "Y"),
               "Your specified target is not binary")
  expect_error(pphcv(df = dat, target = "YCat01", high_card = "hcX",
                     type = "binary", pos_class = 1),
               "Your specified target is not binary")
})

test_that("Correct error is printed when target is binary but there is no
          specified pos_class", {
            
expect_error(pphcv(df = dat, target = "Ycategorical", high_card = "hcX", 
                   type = "binary"), 
             "You must specify the positive class")
})

test_that("warning printed when type is multiclass and target is numeric", {
  
  set.seed(10)
  dat <- data.frame(Ymulticlass = c(1, 2, 4, 3, 1, 2, 4, 3, 1, 2, 3, 4, 4, 7),
                    hcX = sample(letters[1:5], size = 14, replace = TRUE))          
  
  expect_warning(pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
                       type = "multiclass"), 
                 "Your specified target is multiclass and numeric. It was")          
})

test_that("Correct error is thrown when NAs are in the target of HC variable", {
  set.seed(10)
  dat <- data.frame(Ycontinuous = sample(5:42, size = 14, replace = TRUE),
                    Ycategorical = c('Y', "N", NA, "Y", "N", "Y", "N", "Y", NA, 
                                    "Y", "N", "Y", NA, "Y"),
                    YCat01 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0),
                    hcX = sample(letters[1:5], size = 14, replace = TRUE),
                    hcX2 = sample(letters[6:10], size = 14, replace = TRUE),
                    hcX3 = sample(letters[11:15], size = 14, replace = TRUE),
                    Ymulticlass = sample(c("blue", "green", "red"), size = 14, 
                                         replace = TRUE))
  expect_error(pphcv(df = dat, target = "Ycategorical", high_card = "hcX", 
                     type = "binary", pos_class = "Y"), 
               "You must deal with NAs before reducing cardinality. Consider")
})

test_that("Error thrown when m_tuning_param applied during deployment phase", {
  
  f <- pphcv(df = dat, target = "Ycontinuous", high_card = "hcX", 
             type = "continuous")
  
  expect_error(pphcv(df = dat2, high_card = "hcX", mapping = f$mappings,
                     type = "continuous", m_tuning_param = 5), 
               "When applying mappings in testing phase, do not specify an")
})

test_that("Error thrown when user gives positive class in development phase", {
  
  f <- pphcv(df = dat, target = "Ycontinuous", high_card = "hcX", 
             type = "continuous")
  
  expect_error(pphcv(df = dat2, high_card = "hcX", mapping = f$mappings,
                     type = "continuous", pos_class = "Y"), 
         "When applying mappings in testing phase, do not specify a")
})

test_that("Error thrown when user gives multi_base in development phase", {
  
  f <- pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
             type = "multiclass")
  
  expect_error(pphcv(df = dat2, high_card = "hcX", mapping = f$mappings,
                     type = "multiclass", multi_base = "blue"), 
               "When using mappings in testing phase, do not specify a")
})

test_that("Error when user gives different type in training than testing", {
  
  f <- pphcv(df = dat, target = "Ycontinuous", high_card = "hcX", 
             type = "continuous")
  expect_error(pphcv(df = dat2, high_card = "hcX", mapping = f$mappings, 
                     type = "multiclass"),
               "Your specified type does not match the type with which your") 
})

test_that("Error thrown when mapping is not present and there is no target", {
  
  expect_error(pphcv(df = dat, high_card = "hcX", type = "multiclass"), 
             "If mapping is not specified, you must specify a target/outcome.")
})

test_that("Error thrown when mapping and target are included", {
  
  f <- pphcv(df = dat, target = "Ycontinuous", high_card = "hcX", 
             type = "continuous")
  expect_error(pphcv(df = dat, high_card = "hcX", type = "continuous", 
                     target = "Ycontinuous", mapping = f$mappings), 
               "Mapping and target cannot be included in the same step for")
})

test_that("Error thrown when pos_class not specified when type == binary", {
  
  expect_error(pphcv(df = dat, high_card = "hcX", type = "binary", 
                     target = "Ycategorical"), 
               "You must specify the positive class of your binary target")
})
