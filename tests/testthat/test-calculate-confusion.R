context("Checking the functionality of calculateConfusion")

test_that("Two vectors w/o NA give the correct matrix",{
  Type <- c("a","a","a","b","b","b","b","b","c","c","c","c")
  Cluster <- c(1,1,1,1,2,2,2,2,3,3,3,3)
  res <- calculateConfusion(Type,Cluster)
  
  expected <- data.frame(V1=c(1.0,0.2,0.0),
                         V2=c(0.0,0.8,0.0),
                         V3=c(0,0,1))
  rownames(expected) <- c("a","b","c")
  
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("labels vector with NA give the correct matrix",{
  Type <- c("a","a","a","b","b","b","b","b",NA,"c","c","c")
  Cluster <- c(1,1,1,1,2,2,2,2,3,3,3,3)
  res <- calculateConfusion(Type,Cluster)
  
   expected <- data.frame(V1=c(1.0,0.2,0.0),
                          V2=c(0.0,0.8,0.0),
                          V3=c(0,0,1))
  rownames(expected) <- c("a","b","c")
  
  expect_equal(res, expected, tolerance = 1e-6)
})


