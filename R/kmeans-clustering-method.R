#' Build clusters using kmeans()
#'
#' @description This step allows you to use kmeans() to build clusters, based on
#' your data.
#' @docType class
#' @usage KmeansClustering(object, df, grainCol, labelCol, numOfCluster,
#' usePrinComp, numOfPrinComp,impute, debug)
#' @importFrom R6 R6Class
#' @import ranger
#' @import cluster
#' @param object of UnsuperviseModelParameters class for $new() constructor
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol Optional. The dataframe's column that has IDs pertaining to 
#' the grain. No ID columns are truly needed for this step.
#' @param labelCol Optional. 
#' @param numOfCluster Number of clusters you want to build. 
#' @param usePrinComp Optional. TRUE or FALSE. If TRUE, will use the principle components
#' to perform K-means clustering. Default is FALSE.
#' @param numOfPrinComp Optional. Number of principle components you want to use to perform 
#' K-means clustering. 
#' @param impute Set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://hctools.org/}
#' @seealso \code{\link{healthcareai}}
#' @references \url{https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R}
#' @examples
#' 
#' #### Example using iris dataset ####
#' ptm <- proc.time()
#' library(healthcareai)
#' 
#' data(iris)
#' head(iris)
#' 
#' set.seed(2017)
#' 
#' p <- UnsupervisedModelParams$new()
#' p$df <- iris
#' p$labelCol <- "Species"
#' p$impute <- TRUE
#' p$debug <- FALSE
#' p$cores <- 1
#' 
#' # Run k means clustering
#' cl <- KmeansClustering$new(p)
#' cl$run()
#' 
#' # Get the fit result
#' cl$getKmeansfit()
#' 
#' # Get the elbow plot which also presents the optimal number of clusters
#' cl$getElbowPlot()
#' 
#' # Get the 2D representation of the cluster solution
#' cl$get2DClustersPlot()
#' 
#' # Get the sillhouette plot
#' cl$getSilhouettePlot()
#'  
#' # Get a confusion matrix if labelCol exists
#' cl$getConfusionMatrix()
#' 
#' # Get cluster labels
#' cl$getClusterLabels() ## label cluster 1 with "versicolor"
#'                       ## label cluster 2 with "setosa"
#'                       ## label cluster 3 with "virginica"
#' 
#' # Get the output data frame
#' dfOut <- cl$getOutDf()
#' head(dfOut)
#' 
#' ## Write to CSV (or JSON, MySQL, etc) using plain R syntax
#' ## write.csv(dfOut,'path/clusteringresult.csv')
#' 
#' print(proc.time() - ptm)
#' 
#' #### Example using iris data and PCA ####
#' ptm <- proc.time()
#' library(healthcareai)
#' 
#' set.seed(2017)
#' 
#' p <- UnsupervisedModelParams$new()
#' p$df <- iris
#' p$labelCol <- "Species"
#' p$cores <- 1
#' usePrinComp <- TRUE
#' 
#' # Run k means clustering
#' cl <- KmeansClustering$new(p)
#' cl$run()
#' 
#' # Get the scree plot
#' ## This plot presents the fraction of total variance in the data as explained
#' ## or represented by each principle component.
#' cl$getScreePlot()
#' 
#' # According to the scree plot, we may decide to use the first 2 PCs to do clustering.
#' p$usePrinComp <- TRUE
#' p$numOfPrinComp <- 2 ## Not necessary since the default of numOfPrinComp is 2 
#' 
#' # Run k means clustering
#' cl <- KmeansClustering$new(p)
#' cl$run()
#' 
#' # Get the fit result
#' cl$getKmeansfit()
#' 
#' print(proc.time() - ptm)
#'
#' @export

KmeansClustering <- R6Class("KmeansClustering",
                            
  # Inheritance
  inherit = UnsupervisedModel,
  
  # Private members
  private = list(
    # Get kmeans model
    kmeans.fit = NA,
    confusionMatrix = NA,
    optimalNumOfClusters = NA,
    optimalNumOfPCs = NA,
    PCs = NA,
    propVarEx = NA,
    centers = NA,
    cluster = NA,
    outDf = NA,
    clusterLabels = NA,
    mean.vec = NA,
    sd.vec = NA,
    scaledf = NA,
    wss = NA,
    
    
    # Check if the data type is numeric
    checkDataType = function() {
      #print(self$params$dataType)
      if (isNumeric(self$params$df) == FALSE) {
        stop("Your data type must be numeric in order to use kmeans.")
      }
    },
    
    # Perform clustering
    performClustering = function() {
      if (isTRUE(self$params$debug)) {
        print('scale the data set for k means clustering...')
      }
      # Scale data
      scaleRes <- dataScale(self$params$df)
      private$mean.vec <- scaleRes[[1]]
      private$sd.vec <- scaleRes[[2]]
      private$scaledf <- scaleRes[[3]]
      
      # Find the optimal number of clusters
      k.max <- 15 # Maximal number of clusters
      private$wss <- sapply(1:k.max, 
                            function(k){kmeans(private$scaledf, k, nstart = 10 )$tot.withinss})
      private$optimalNumOfClusters <- findElbow(private$wss)
      
      # PCA
      pcaRes <- pcaAnalysis(self$params$df)
      private$PCs <- pcaRes[[1]]
      private$propVarEx <- pcaRes[[2]]
      
      # Find the optimal number of PCs
      private$optimalNumOfPCs <- findElbow(private$propVarEx)
      
      # Use principle components if usePrinComp is TRUE 
      if (self$params$usePrinComp == TRUE && is.null(self$params$numOfPrinComp)) {
        df <- private$PCs[,1:private$optimalNumOfPCs]
      } else if (self$params$usePrinComp == TRUE && !is.null(self$params$numOfPrinComp)) {
        df <- private$PCs[,1:self$params$numOfPrinComp]
      } else {
        df <- private$scaledf
      }
      
      # Build clusters
      ## If the numOfClusters is not given, use the optimalNumOfClusters generated 
      ## by findElbow()
      if (!is.null(self$params$numOfClusters)) {
        numOfClusters <- self$params$numOfClusters 
      } else {
        numOfClusters <- private$optimalNumOfClusters
      }
      # Run K-Means and save the result
      private$kmeans.fit <- kmeans(df, numOfClusters)
      # Save the centers and clusters
      private$centers <- private$kmeans.fit[["centers"]]
      private$cluster <- private$kmeans.fit[["cluster"]]
      
      # Calculate confusion matrix and assign label to the clusters 
      # if labelCol exists
      if (nchar(self$params$labelCol) != 0) {
        private$confusionMatrix <- calculateConfusion(private$labelColValues, private$cluster)
        private$clusterLabels <- assignClusterLabels(private$confusionMatrix, 
                                                     nrow(private$centers))
      }
      
    },
    
    ## Generate the data frame that Combine grain.col, cluster labels, 
    ## and time to be put back into SAM table
    createDf = function() {
      dtStamp <- as.POSIXlt(Sys.time())
      
      private$outDf <- data.frame(
        0,                                 # BindingID
        'R',                               # BindingNM
        dtStamp,                           # LastLoadDTS
        private$grainColValues,            # GrainID
        private$labelColValues,            # labelCol
        private$cluster)                   
      
      
      colnames(private$outDf) <- c(
        "BindingID",
        "BindingNM",
        "LastLoadDTS",
        self$params$grainCol,
        self$params$labelCol,
        "cluster"
      )
      
      # Remove columns that are only NA
      private$outDf <- removeColsWithOnlyNA(private$outDf)
      
      # Remove row names so df can be written to DB
      rownames(private$outDf) <- NULL
      
      if (isTRUE(self$params$debug)) {
        cat('Dataframe with predictions:', '\n')
        cat(str(private$outDf), '\n')
      }
    }
    
  ),
  
  # Public members
  public = list(
    
    # Constructor
    # p: new SuperviseModelParameters class object,
    # i.e. p = SuperviseModelParameters$new()
    initialize = function(p) {
      set.seed(43)
      super$initialize(p)
    },
    
    # Override: run k-means algorithm
    run = function() {
      
      # Check data type
      private$checkDataType()
      
      # Clustering
      private$performClustering()
      
      # Generate the df ready for output
      private$createDf()
      
    },
    
    ## TODO: missing values?
    getLabelOfNewdf = function(x) {
      ## Load data, only use the columns that used in kmeans clustering
      x <- x[,names(self$params$df)]
      for (i in 1:nrow(x)) {
        x[i,] <- (x[i,] - private$mean.vec)/private$sd.vec
      }
      # Compute squared euclidean distance from each sample to each cluster center
      tmp <- sapply(seq_len(nrow(x)),
                    function(i) apply(private$centers, 1,
                                      function(v) sum((x[i, ] - v)^2)))
      max.col(-t(tmp))  # find index of min distance
    },
    
    
    # Plot scree plot
    getScreePlot = function() {
      plot(private$propVarEx, xlab = "Principal Component",
           ylab = "Proportion of Variance Explained", type = "b", pch = 19)
      abline(v = private$optimalNumOfPCs, lty = 2)
    },
    
    # Generawte elbow plot for k = 2 to k = 15
    getElbowPlot = function(df) {
      plot(1:15, private$wss,
           type = "b", pch = 19, frame = FALSE, 
           xlab = "Number of clusters K",
           ylab = "Total within-clusters sum of squares")
      abline(v = private$optimalNumOfClusters, lty = 2)
    },
    
    # Plot 2D cluster solution
    get2DClustersPlot = function() {
      PC2s <- private$PCs[,1:2]
      cluster <- private$cluster
      D <- cbind(PC2s, cluster)
      plot(D$PC1, D$PC2, col = D$cluster, pch = 16, cex = 1.1,
           xlab = "Component 1", 
           ylab = "Component 2",
           main = "2D representation of the cluster solution",
           sub = paste("These two components explain",
                       format(100*sum(private$propVarEx[1:2]),digit = 3),
                       "% of the point variability"))
    },
    
    # Plot Silhouette plot
    getSilhouettePlot = function() {
      dis <- dist(private$scaledf, method = "euclidean")
      plot(silhouette(private$cluster,dis))
    },
    
    getOutDf = function() {
      return(private$outDf)
    },
    
    getKmeansfit = function() {
      return(private$kmeans.fit)
    },
    
    getConfusionMatrix = function() {
      return(private$confusionMatrix)
    },
    
    getClusterLabels = function() {
      return(private$clusterLabels)
    }
    
  )
  
)

