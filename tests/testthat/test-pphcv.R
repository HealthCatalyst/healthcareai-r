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

test_that("For a continuous target, one high-cardinality variable, and no 
          specified m, pphcv returns correct, new feature", {

  b <- pphcv(df = dat, target = "Ycontinuous", high_card = "hcX", 
             type = "continuous")
  
  expect_equal(b$newdf$hcX_new[1], 22.05357, tolerance = 0.001)
  expect_equal(b$newdf$hcX_new[12], 20.55357, tolerance = 0.001)
})

test_that("For a binary target, one high-cardinality variable, one specified
          positilve class and no specified m, pphcv returns correct, new 
          feature", {
            
  b <- pphcv(df = dat, target = "Ycategorical", high_card = "hcX", 
             type = "binary", pos_class = "Y")
            
  expect_equal(b$newdf$hcX_new[5], 0.1428571, tolerance = 0.001)
  expect_equal(b$newdf$hcX_new[14], 0.3571429, tolerance = 0.001)
})

test_that("For a binary target coded as continuous(0,1), one 
          high-cardinality variable, the positilve class and no specified m, 
          pphcv returns correct, new feature", {
            
 b <- pphcv(df = dat, target = "YCat01", high_card = "hcX", type = "binary", 
            pos_class = 1)
            
 expect_equal(b$newdf$hcX_new[5], 0.1428571, tolerance = 0.001)
 expect_equal(b$newdf$hcX_new[14], 0.3571429, tolerance = 0.001)
})

test_that("For a binary target coded as numeric(0,1), one 
          high-cardinality variable, type = 'continuous', no specified 
          pos_class, and no specified m, pphcv returns correct, new feature", {
            
 b <- pphcv(df = dat, target = "YCat01", high_card = "hcX", type = "continuous")
            
 expect_equal(b$newdf$hcX_new[5], 0.1428571, tolerance = 0.001)
 expect_equal(b$newdf$hcX_new[14], 0.3571429, tolerance = 0.001)
})

test_that("For a multiclass target , one high-cardinality variable, and 
          type = 'multiclass', no specified pos_class, and no specified m, pphcv
          returns correct, new features and correct number of new features", {
            
 b <- pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
            type = "multiclass")
            
 expect_equal(b$newdf$hcX_green_new[7], 0.875, tolerance = 0.001)
 expect_equal(b$newdf$hcX_green_new[10], 0.375, tolerance = 0.001)
 expect_equal(b$newdf$hcX_red_new[7], 0.03571429, tolerance = 0.001)
 expect_equal(b$newdf$hcX_red_new[8], 0.5357143, tolerance = 0.001)
 expect_equal((length(dat) + length(levels(dat$Ymulticlass)) - 1), 
               length(b$newdf), tolerance = 0)
})

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

test_that("For a multiclass target, a vector of high-cardinality variables and
          type = 'multiclass', pphcv returns correct new features and correct
          number of new features", {
 
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

test_that("when specifying an m, pphcv returns correct new feature", {
  
  b <- pphcv(df = dat, target = "Ycategorical", high_card = "hcX", 
             type = "binary", m = 10, pos_class = "Y")
  
  expect_equal(b$newdf$hcX_new[7], 0.4835165, tolerance = 0.001)
  expect_equal(b$newdf$hcX_new[14], 0.4065934, tolerance = 0.001)
})

test_that("Correct error is printed when target is binary and numeric but 
          specified pos_class is not numeric", {
            
 expect_error(pphcv(df = dat, target = "YCat01", high_card = "hcX", 
                    type = "binary", pos_class = "Y"), 
              "If specified binary target is numeric,")
})

test_that("Correct error is printed when target is binary but there is no
          specified pos_class", {
            
expect_error(pphcv(df = dat, target = "Ycategorical", high_card = "hcX", 
                   type = "binary"), 
             "You must specify the positive class")
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
               "You must deal with NAs before reducing cardinality")
})

test_that("warning about coercion is thrown when type is multiclass and target
          is numeric", {
 set.seed(10)
 dat <- data.frame(Ymulticlass = c(1, 2, 4, 3, 1, 2, 4, 3, 1, 2, 3, 4, 4, 7),
                   hcX = sample(letters[1:5], size = 14, replace = TRUE))          
  
 expect_warning(pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
                    type = "multiclass"), 
              "Your specified target is multiclass and numeric.")
})