context("Testing kmeans clustering and data prep")
# Unlabeled Tests:
# 1. Auto-clustering works, finds 5 clusters, and agrees with WSS
# 2. Clustering works with specified 3 clusters
# 3. Clustering works with auto-PCA
# 4. Clustering works with PCA and specified number of PCs
# 5. Providing categorical columns throws error
# Labeled Tests:
# 1. Auto-clustering works with label
# 2. Auto-clustering throws error number of clusters and number of labels mismatch.
# 3. Auto-clustering works with label and PCA

##################################################################
##################################################################
# Unlabeled tests
library(healthcareai)
csvfile <- system.file("extdata", 
                       "HCRDiabetesClinical.csv", 
                       package = "healthcareai")
df <- read.csv(file = csvfile, 
               header = TRUE, 
               na.strings = c("NULL", "NA", ""))
df$PatientID <- NULL

test_that("Auto-clustering works and finds 5 clusters",{
  closeAllConnections()
  set.seed(2017)
  p <- UnsupervisedModelParams$new()
  p$df <- df
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1

  cl <- KmeansClustering$new(p)
  junk <- capture.output(cl$run())
  dfOut <- cl$getOutDf()
  kFit <- cl$getKmeansFit()
   
  # Find 5 clusters 
  expect_equal(max(dfOut$assignedCluster), 
               5,
               tolerance=1e-3)
   
  # Total within cluster sum of squares is as expected 
  expect_equal(kFit$tot.withinss, 
               3328.921,
               tolerance=1e-3)
})

test_that("Clustering works with specified 3 clusters",{
  closeAllConnections()
  set.seed(2017)
  p <- UnsupervisedModelParams$new()
  p$df <- df
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1
  p$numOfClusters <- 3

  cl <- KmeansClustering$new(p)
  junk <- capture.output(cl$run())
  dfOut <- cl$getOutDf()
  kFit <- cl$getKmeansFit()
   
  # Find 5 clusters 
  expect_equal(max(dfOut$assignedCluster), 
               3,
               tolerance=1e-3)
   
  # Total within cluster sum of squares is as expected 
  expect_equal(kFit$tot.withinss, 
               4051.994,
               tolerance=1e-3)
})

test_that("Clustering works with auto-PCA",{
  closeAllConnections()
  # This dataset triggers the non-concave warning and defaults to 2 PCs.
  set.seed(2017)
  p <- UnsupervisedModelParams$new()
  p$df <- df
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1
  p$usePCA <- TRUE

  cl <- KmeansClustering$new(p)
  suppressWarnings(junk <- capture.output(cl$run()))
  dfOut <- cl$getOutDf()
  kFit <- cl$getKmeansFit()
   
  # Find 5 clusters 
  expect_equal(max(dfOut$assignedCluster), 
               5,
               tolerance=1e-3)
   
  # Total within cluster sum of squares is as expected 
  expect_equal(kFit$tot.withinss, 
               648.2361,
               tolerance=1e-3)

  # number of clusters is 5 and number of features is 2. 
  expect_equal(dim(kFit$centers), 
               c(5,2),
               tolerance=1e-3)
  # 2nd feature is named 'PC2'
  expect_equal(colnames(kFit$centers)[2], 
               'PC2')
})

test_that("Clustering works with PCA and specified number of PCs",{
  closeAllConnections()
  set.seed(2017)
  p <- UnsupervisedModelParams$new()
  p$df <- df
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1
  p$usePCA <- TRUE
  p$numOfPCA <- 3

  cl <- KmeansClustering$new(p)
  junk <- capture.output(cl$run())
  dfOut <- cl$getOutDf()
  kFit <- cl$getKmeansFit()
   
  # Find 5 clusters 
  expect_equal(max(dfOut$assignedCluster), 
               5,
               tolerance=1e-3)
   
  # Total within cluster sum of squares is as expected 
  expect_equal(kFit$tot.withinss, 
               1306.609,
               tolerance=1e-3)

  # number of clusters is 5 and number of features is 3. 
  expect_equal(dim(kFit$centers), 
               c(5,3),
               tolerance=1e-3)
  # 2nd feature is named 'PC2'
  expect_equal(colnames(kFit$centers)[3], 
               'PC3')
})

test_that("Providing categorical columns throws error", {
  df2 <- data.frame(a=1:10,
                    b=31:40,
                    c=c('chef','chef','chef','elmo','elmo','elmo','bert','bert','oscar','oscar'))
  p <- UnsupervisedModelParams$new()
  p$df <- df2
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1
  cl <- KmeansClustering$new(p)
  # column c is a factor
  expect_error(cl$run())

  df2$c <- as.character(df2$c)
  p <- UnsupervisedModelParams$new()
  p$df <- df2
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1
  cl <- KmeansClustering$new(p)
  # column c is now a character
  expect_error(cl$run())
})


##################################################################
##################################################################
# Labeled tests
data(iris)

test_that("Auto-clustering works with label",{
  closeAllConnections()
  set.seed(2017)
  p <- UnsupervisedModelParams$new()
  p$df <- iris
  p$labelCol <- 'Species'
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1

  junk <- capture.output(cl <- KmeansClustering$new(p))
  junk <- capture.output(cl$run())
  dfOut <- cl$getOutDf()
  kFit <- cl$getKmeansFit()
  cm <- cl$getConfusionMatrix()
   
  # Find creates correct output DF
  expect_equal(dim(dfOut), 
               c(150,6),
               tolerance=1e-3)
  expect_equal(as.character(dfOut$trueGroup[1]), 
               'setosa')
   
  # Total within cluster sum of squares is as expected 
  expect_equal(kFit$tot.withinss, 
               138.8884,
               tolerance=1e-3)

  # Confusion matrix is created and sorted correctly
  expect_equal(cm[1,1], 
               1,
               tolerance=1e-2)
  expect_equal(cm[3,3], 
               .72,
               tolerance=1e-2)
})

test_that("Autoclustering throws error number of clusters and number of labels mismatch.",{
  closeAllConnections()
  set.seed(2017)
  p <- UnsupervisedModelParams$new()
  p$df <- iris
  p$labelCol <- 'Species'
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1
  p$numOfClusters <- 2

  junk <- capture.output(cl <- KmeansClustering$new(p))
  
  # Find creates correct output DF
  expect_error(cl$run(),
    'If you are using a labeled dataset, you must use the same number of clusters and labels')
})

test_that("Auto-clustering works with label and PCA",{
  closeAllConnections()
  set.seed(2017)
  p <- UnsupervisedModelParams$new()
  p$df <- iris
  p$labelCol <- 'Species'
  p$impute <- TRUE
  p$debug <- FALSE
  p$cores <- 1
  p$usePCA <- TRUE

  junk <- capture.output(cl <- KmeansClustering$new(p))
  junk <- capture.output(cl$run())
  dfOut <- cl$getOutDf()
  kFit <- cl$getKmeansFit()
  cm <- cl$getConfusionMatrix()
   
  # Find creates correct output DF
  expect_equal(dim(dfOut), 
               c(150,6),
               tolerance=1e-3)
  expect_equal(as.character(dfOut$trueGroup[1]), 
               'setosa')
   
  # Total within cluster sum of squares is as expected 
  expect_equal(kFit$tot.withinss, 
               114.254,
               tolerance=1e-3)

  # Confusion matrix is created and sorted correctly
  expect_equal(cm[1,1], 
               1,
               tolerance=1e-2)
  expect_equal(cm[3,3], 
               .72,
               tolerance=1e-2)

  # number of clusters is 3 and number of features is 2. 
  expect_equal(dim(kFit$centers), 
               c(3,2),
               tolerance=1e-3)
  # 2nd feature is named 'PC2'
  expect_equal(colnames(kFit$centers)[2], 
               'PC2')
})
