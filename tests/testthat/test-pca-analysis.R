context("Checking the functionality of pcaAnalysis")

test_that("A numeric data frame w/o NA's gives the correct result",{
  df <- data.frame(a = c(1,2,3,8,2,3,5,6,7,4,5,3),
                   b = c(2,3,2,3,5,1,2,3,5,3,4,6),
                   c = c(6,5,8,9,0,8,9,8,6,7,8,7))
  
  res <- pcaAnalysis(df)
  
  PCs <- data.frame(PC1 = c(-0.59115615,-0.94943186,0.48740769,1.68245244,
                            -3.04645071,0.78115810,1.26406792,0.90579221,
                            -0.04813752,0.12913198,0.37466348,-0.98949758),
                    PC2 = c(1.5764451,0.7560320,0.9738239,-1.0695203,
                            -0.2910173,1.4752377,0.3623583,-0.4580548,
                            -1.7887260,0.1534108,-0.6493137,-1.0406758),
                    PC3 = c(0.146245777,-0.038811374,0.189324503,-0.456617160,
                            -0.725525385,-0.151120945,-0.041117752,-0.226174904,
                            -0.344307589,0.004267352,0.366252163,1.277585315))
  prop_of_var <- c(0.53839293,0.37814531,0.08346177)
  expected <- list(PCs, prop_of_var)
  names(expected) <- c("PCs","prop_of_var")
  
  expect_equal(res,expected,tolerance = 1e-6)
})

test_that("A numeric data frame with NA's gives correct error",{
  df <- data.frame(a = c(1,2,3,8,2,3,5,6,7,4,5,3),
                   b = c(2,3,2,3,5,1,2,3,5,3,4,6),
                   c = c(6,5,8,9,0,8,9,NA,6,7,8,7))
  
  expect_error(pcaAnalysis(df), # <-- error
                         "Missing values in data frame")
})