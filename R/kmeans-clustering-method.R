#' Build clusters using kmeans()
#'
#' @description This step allows you to use kmeans() to build clusters, based on
#' your data.
#' @docType class
#' @usage KmeansClustering(object, df, grainCol, labelCol, numOfClusters,
#' usePrinComp, numOfPrinComp,impute, debug)
#' @importFrom R6 R6Class
#' @import ranger
#' @param object of UnsuperviseModelParameters class for $new() constructor
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol Optional. The dataframe's column that has IDs pertaining to 
#' the grain. No ID columns are truly needed for this step.
#' @param labelCol Optional. Labels will not be used for clustering, but if the 
#' data are labeled, this can be used for validation. Functions getClusterLabels()
#' and getConfusionMatrix() are only available if labelCol is provided. Also supervised 
#' models might be a better choice if labelCol is provided and the goal is classification.
#' @param numOfClusters Number of clusters you want to build. If left blank, will 
#' be determined automatically from the elbow plot.
#' @param usePrinComp Optional. TRUE or FALSE. Default is FALSE. If TRUE, the method 
#' will use the principle components returned by princomp() as the new features to 
#' perform K-means clustering. This may accelerate convergence on high-dimension
#' datasets. If TRUE, getParallelCoordinatePlot() can no longer be used to expalin how variables 
#' contributed in each cluster.
#' @param numOfPrinComp Optional. If usePrinComp is TRUE, you need to decide how 
#' many principle components you want to use to perform K-means clustering. If left 
#' blank, it will be determined automatically from the scree plot. 
#' @param impute Set all-column imputation to FALSE or TRUE.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' FALSE leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use TRUE or FALSE.
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
    kmeansFit = NA,
    scaledDf = NA,
    confusionMatrix = NA,
    optimalNumOfClusters = NA,
    optimalNumOfPCs = NA,
    PCs = NA,
    propVarEx = NA,
    centers = NA,
    cluster = NA,
    outDf = NA,
    clusterLabels = NA,
    meanVec = NA,
    sdVec = NA,
    dfCls = NA,
    wss = NA,
    
    # Check if the data type is numeric
    checkDataType = function() {
      #print(self$params$dataType)
      if (isNumeric(self$params$df) == FALSE) {
        stop("Your data type must be numeric in order to use kmeans.")
      }
    },

    performPCA = function() {
    # Calculate principle components for plotting.
      if (isTRUE(self$params$debug)) {
        print('Doing principle component analysis...')
      }
      pcaRes <- pcaAnalysis(self$params$df)
      private$PCs <- pcaRes[['PCs']]
      private$propVarEx <- pcaRes[['prop_of_var']]

      if (self$params$usePrinComp == FALSE) {
        private$dfCls <- private$scaledDf

      # PCA=TRUE and user specified number of PCs to use
      } else if (!is.null(self$params$numOfPrinComp)) {
        private$dfCls <- private$PCs[,1:self$params$numOfPrinComp]
      
      # PCA=TRUE and user didn't specify number of PCs to use. Calculate from elbow plot.
      } else if (is.null(private$optimalNumOfPCs)) {
        if (isTRUE(self$params$debug)) {
          print('Finding the optimal number of principle components...')
        }
        if (length(private$propVarEx) <= 2) {
          private$optimalNumOfPCs = length(private$propVarEx)
        } else {
          private$optimalNumOfPCs <- findElbow(private$propVarEx)
        }
        private$dfCls <- private$PCs[,1:private$optimalNumOfPCs]
      }
    },

    kmeansConfusionMatrix = function() {
      # TODO replace with xgb shared function.
      private$confusionMatrix <- calculateConfusion(private$labelColValues, private$cluster)
      private$clusterLabels <- assignClusterLabels(private$confusionMatrix)
      # Apply labels and sort.
      names(private$confusionMatrix) <- private$clusterLabels
      private$confusionMatrix <- private$confusionMatrix[ , order(names(private$confusionMatrix))]
    },
    
    # Perform clustering
    performClustering = function() {
      if (isTRUE(self$params$debug)) {
        print('scale the data set for k means clustering...')
      }
      # Scale data
      scaleRes <- dataScale(self$params$df)
      private$meanVec <- scaleRes[['means']]
      private$sdVec <- scaleRes[['standard_deviations']]
      private$scaledDf <- scaleRes[['scaled_df']]

      # If the numOfClusters is not given, calculate optimal NumOfClusters 
      # from findElbow()
      if (!is.null(self$params$numOfClusters)) {
        numOfClusters <- self$params$numOfClusters 
      } else {
        # Find the optimal number of clusters
        maxClusters <- 15 # Maximal number of clusters
        private$wss <- sapply(1:maxClusters, 
                              function(k){kmeans(private$scaledDf, k, nstart = 3)$TotalWithinSS})
        private$optimalNumOfClusters <- findElbow(private$wss)
        if (isTRUE(self$params$debug)) {
          print('Finding the optimal number of clusters...')
        }
        numOfClusters <- private$optimalNumOfClusters
      }
      
      # Do PCA for plotting.
      # Default to skipping PCA.
      private$performPCA()

      # Build clusters
      # Run K-Means and save the result
      if (isTRUE(self$params$debug)) {
        print('Building clusters...')
      }
      private$kmeansFit <- kmeans(private$dfCls, numOfClusters, nstart = 10)
      # Save the centers and clusters
      private$centers <- private$kmeansFit[["centers"]]
      private$cluster <- private$kmeansFit[["cluster"]]

      # Calculate confusion matrix and assign label to the clusters 
      # if labelCol exists
      if (nchar(self$params$labelCol) != 0) {
        if (isTRUE(self$params$debug)) {
          print('Generating confusion matrix...')
        }
        private$kmeansConfusionMatrix()
        cat('Confusion matrix for cluster assignment. Given as percentage. \n')
        print(private$confusionMatrix)
        cat('\n')

        from <- 1:length(unique(private$labelColValues))
        to <- private$clusterLabels
        map = setNames(to,from)
        private$cluster <- map[private$cluster]
      }

      # prepare performance metrics
      clusterOrder <- order(private$clusterLabels)
      self$performance <- data.frame(clusterNum=1:length(private$clusterLabels),
                          clusterName=private$clusterLabels[clusterOrder],
                          withinClusterSS=private$kmeansFit$withinss[clusterOrder])

      # Print performance metrics
      cat('Total sum of squares errors within clusters is:', 
        private$kmeansFit$tot.withinss, '\n')
      cat('Sum of squares errors in each cluster is:\n')
      print(self$performance)
    },
    
    ## Generate the data frame that Combine grain.col, cluster labels, 
    ## and time to be put back into SAM table
    # TODO add a distance metric to the output.
    createDf = function() {
      dtStamp <- as.POSIXlt(Sys.time())

      # If a label was provided
      if (nchar(self$params$labelCol) != 0) {
        if (isTRUE(self$params$debug)) {
          print('Generating output dataframe using labels in labelCol...')
        }
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
          "Grain",
          "trueCluster",
          "predictedCluster"
        )
      # No label column
      } else {
        if (isTRUE(self$params$debug)) {
        print('Generating output dataframe without labels...')
        }
        private$outDf <- data.frame(
          0,                                 # BindingID
          'R',                               # BindingNM
          dtStamp,                           # LastLoadDTS
          private$grainColValues,            # GrainID
          private$cluster)                   
        
        colnames(private$outDf) <- c(
          "BindingID",
          "BindingNM",
          "LastLoadDTS",
          "Grain",
          "assignedCluster"
        )
      }
      
      # Remove columns that are only NA
      private$outDf <- removeColsWithOnlyNA(private$outDf)
      
      # Remove row names so df can be written to DB
      rownames(private$outDf) <- NULL
      
      if (isTRUE(self$params$debug)) {
        cat('Dataframe with clustering result:', '\n')
        cat(str(private$outDf), '\n')
      }
    }
  ),
  
  # Public members
  public = list(
    performance = NA,
    
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
    
    # Plot scree plot
    getScreePlot = function() {
      plot(private$propVarEx, xlab = "Principal Component",
           ylab = "Proportion of Variance Explained", type = "b", pch = 19)
      abline(v = private$optimalNumOfPCs, lty = 2)
    },
    
    # Generate elbow plot for k = 2 to k = 15
    getElbowPlot = function() {
      plot(1:15, private$wss,
           type = "b", pch = 19, frame = FALSE, 
           xlab = "Number of clusters K",
           ylab = "Total within-clusters sum of squares")
      abline(v = private$optimalNumOfClusters, lty = 2)
    },
    
    # Plot 2D cluster solution
    get2DClustersPlot = function(label = FALSE) {
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
      legend(x='topright', legend=self$performance$clusterName, col=self$performance$clusterName, pch=16)
      if (label == TRUE) {
        if (nchar(self$params$grainCol) == 0) 
          stop("Grain IDs are not available since no grainCol is provided")
        else text(D$PC1, D$PC2, labels = private$grainColValues, cex = 0.7, pos = 3)
      }
    },
    
    getConfusionMatrix = function() {
      if (nchar(self$params$labelCol) == 0) 
        stop("This function is unavailable since no labelCol is provided.")
      else return(private$confusionMatrix)
    },

    # Plot parallel coordinates plot to see how variables contributed in each cluster
    getParallelCoordinatePlot = function() {
      if (isTRUE(self$params$usePrinComp))
        stop("This function is not available since principle components are used as 
             new features to perform kmean clustering")
      else MASS::parcoord(private$dfCls, private$cluster)
    },
    
    getOutDf = function() {
      return(private$outDf)
    },
    
    getKmeansfit = function() {
      return(private$kmeansFit)
    }
  )
)

