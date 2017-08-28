#' Build clusters using kmeans()
#' 
#' @description This step allows you to use kmeans clustering to explore and
#'   group your data.
#' @docType class
#' @usage KmeansClustering(object, df, grainCol, labelCol, numOfClusters, 
#'   usePCA, numOfPCA,impute, debug)
#' @importFrom R6 R6Class
#' @importFrom stats lm sd coef prcomp
#' @param object of UnsupervisedModelParams class for $new() constructor
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol Optional. The dataframe's column that has IDs pertaining to 
#'   the grain. No ID columns are truly needed for this step. If left blank, row
#'   numbers are used for identification.
#' @param labelCol Optional. Labels will not be used for clustering. Labels can
#'   be can be used for validation. The number of clusters should be the same as
#'   the number of labels. Functions getClusterLabels() and getConfusionMatrix()
#'   are only available if labelCol is provided. Generally, supervised models
#'   are a better choice if your goal is classification.
#' @param numOfClusters Number of clusters you want to build. If left blank,
#'   will be determined automatically from the elbow plot.
#' @param usePCA Optional. TRUE or FALSE. Default is FALSE. If TRUE, the
#'   method will use principle components as the new features to perform K-means
#'   clustering. This may accelerate convergence on high-dimension datasets.
#' @param numOfPCA Optional. If using principle components, you may specify
#'   the number to use to perform K-means clustering. If left blank, it will be
#'   determined automatically from the scree (elbow) plot.
#' @param impute Set all-column imputation to FALSE or TRUE. This uses mean
#'   replacement for numeric columns and most frequent for factorized columns. 
#'   FALSE leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order to
#'   monitor the calculations throughout. Use TRUE or FALSE.
#'   
#' @details This is an unsupervised method for clustering data. That is, no
#'   response variable is needed or used. If you want to examine how the data
#'   clusters by some labeled grouping, you can specify the grouping in
#'   \code{labelCol}, but the labels are not used in
#'   the clustering process. If you want to use labels to train the model 
#'   see \code{\link{LassoDevelopment}} or \code{\link{RandomForestDevelopment}}.
#'   
#' @section Methods: The above describes params for initializing a new
#'   KmeansClustering class with \code{$new()}. Individual methods are
#'   documented below.
#' @section \code{$new()}: Initializes a new Kmeans Clustering class using the 
#'   parameters saved in \code{p}, documented above. This method loads, cleans,
#'   and prepares data for clustering. \cr \emph{Usage:} \code{$new(p)}
#' @section \code{$run()}: Calculates clusters, displays performance. \cr 
#'   \emph{Usage:}\code{$run()}
#' @section \code{$get2DClustersPlot()}: Displays the data and assigned
#'   clusters. PCA is used to visualize the top two priciple components for
#'   plotting. This is unrelated to variable reduction for clustering. Passing 
#'   TRUE to this function will display grain IDs on the plot. \cr 
#'   \emph{Usage:} \code{$get2DClustersPlot()} \cr
#' @section \code{$getOutDf()}: Returns the output dataframe for writing to SQL
#'   or CSV. \cr \emph{Usage:} \code{$getOutDf()} \cr
#' @section \code{$getConfusionMatrix()}: Returns a confusion matrix of assigned
#'   cluster vs. provided labels. Clusters are named based on maximum overlap
#'   with label. Only available if labelCol is specified. Rows are true labels,
#'   columns are assigned clusters. \cr 
#' \emph{Usage:} \code{$getConfusionMatrix()} \cr
#' @section \code{$getElbowPlot()}: Plots total within cluster error vs. number
#'   of clusters. Available if the number of clusters is unspecified. \cr 
#'   \emph{Usage:} \code{$getElbowPlot()} \cr
#' @section \code{$getScreePlot()}: Plots total variance explained vs. number of
#'   principle components. Available if the number of principle components is
#'   unspecified. \cr \emph{Usage:} \code{$getScreePlot()} \cr
#' @section \code{$getKmeansFit()}: Returns all attributes of the kmeans fit
#'   object. \cr \emph{Usage:} \code{$getKmeansFit()} \cr
#' @references \url{http://hctools.org/}
#' @seealso \code{\link{healthcareai}}
#' @references
#'   \url{https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R}
#'   
#' @examples
#' 
#' #### Example using Diabetes dataset ####
#' ptm <- proc.time()
#' # Can delete this line in your work
#' csvfile <- system.file("extdata", 
#'                        "HCRDiabetesClinical.csv", 
#'                        package = "healthcareai")
#' # Replace csvfile with 'your/path'
#' df <- read.csv(file = csvfile, 
#'                header = TRUE, 
#'                na.strings = c("NULL", "NA", ""))
#' head(df)
#' df$PatientID <- NULL
#' 
#' set.seed(42)
#' p <- UnsupervisedModelParams$new()
#' p$df <- df
#' p$impute <- TRUE
#' p$grainCol <- "PatientEncounterID"
#' p$debug <- FALSE
#' p$cores <- 1
#' p$numOfClusters <- 3
#' 
#' # Run k means clustering
#' cl <- KmeansClustering$new(p)
#' cl$run()
#' 
#' # Get the 2D representation of the cluster solution
#' cl$get2DClustersPlot()
#' 
#' # Get the output data frame
#' dfOut <- cl$getOutDf()
#' head(dfOut) 
#' 
#' print(proc.time() - ptm)
#' 
#' 
#' 
#' 
#' #### Example using iris dataset with labels ####
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
#' p$labelCol <- 'Species'
#' p$impute <- TRUE
#' p$debug <- FALSE
#' p$cores <- 1
#' 
#' # Run k means clustering
#' cl <- KmeansClustering$new(p)
#' cl$run()
#' 
#' # Get the 2D representation of the cluster solution
#' cl$get2DClustersPlot()
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
      a <- lapply(self$params$df,is.numeric)
      if (all(unlist(a)) == FALSE) {
        stop("Kmeans requires a dataframe with numeric columns. Remove non-numeric columns 
          and categorical columns with more than 2 categories.")
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

      if (self$params$usePCA == FALSE) {
        private$dfCls <- private$scaledDf

      # PCA=TRUE and user specified number of PCs to use
      } else if (!is.null(self$params$numOfPCA)) {
        
        # Ensure number PCs to use is not greater than number of variables
        if (ncol(private$PCs) < self$params$numOfPCA)
            stop("numOfPCA must be less than the number of variables on which to cluster.")
        private$dfCls <- private$PCs[,1:self$params$numOfPCA]
      
      # PCA=TRUE and user didn't specify number of PCs to use. Calculate from elbow plot.
      } else {
        if (isTRUE(self$params$debug)) {
          print('Finding the optimal number of principle components...')
        }
        if (length(private$propVarEx) <= 2) {
          private$optimalNumOfPCs <- length(private$propVarEx)
        } else {
          private$optimalNumOfPCs <- findElbow(private$propVarEx)
        }
        private$dfCls <- private$PCs[,1:private$optimalNumOfPCs]
      }
      if (isTRUE(self$params$debug)) {
          cat('Using', private$optimalNumOfPCs,' principle components. \n')
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
      } else if (nchar(self$params$labelCol) != 0) {
        numOfClusters <- length(unique(private$labelColValues))
        if (isTRUE(self$params$debug)) {
          cat('Clustering on', numOfClusters,',as label column contains', numOfClusters,'unique 
            labels. If you want less/more clusters, remove the label column. \n')
        }
      } else {
        # Find the optimal number of clusters
        maxClusters <- 15 # Maximal number of clusters
        private$wss <- sapply(1:maxClusters, 
                              function(k){kmeans(private$scaledDf, k, nstart = 3)$tot.withinss})
        private$optimalNumOfClusters <- findElbow(private$wss)
        numOfClusters <- private$optimalNumOfClusters

        if (isTRUE(self$params$debug)) {
          print('Finding the optimal number of clusters...')
          cat('Optimal number of clusters is:', numOfClusters,'. Look at elbow plot with
            $getElbowPlot to see. \n')
        }
      }

      # Set cluster labels to numbers if a label column wasn't specified.
      if (nchar(self$params$labelCol) == 0) {
        private$clusterLabels <- 1:numOfClusters 
      }
      
      # Do PCA for plotting. If $usePCA==TRUE, it will use PCA data for clustering.
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
        cat('Confusion matrix for cluster assignment, given as proportion correct. \n
          Columns are assigned clusters, Rows are true labels.')
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
      if (nchar(self$params$grainCol)==0) {
        self$params$grainCol <- 'Grain'
      }
      # If a label was provided
      if (nchar(self$params$labelCol) != 0) {
        if (isTRUE(self$params$debug)) {
          print('Generating output dataframe using labels in labelCol...')
        }
        private$outDf <- data.frame(
          0,                                 # BindingID
          'R',                               # BindingNM
          dtStamp,                           # LastLoadDTS
          private$grainColValues,            # Grain
          private$labelColValues,            # labelCol
          private$cluster)                   # assigned cluster
        
        colnames(private$outDf) <- c(
          "BindingID",
          "BindingNM",
          "LastLoadDTS",
          self$params$grainCol,
          "trueGroup",
          "assignedCluster"
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
          private$grainColValues,            # Grain
          private$cluster)                   # assigned cluster
        
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
      # Make sure number clusters wasn't provided
      if (!is.null(self$params$numOfClusters))
        stop("Elbow plots compare number of clusters, but you provided a number with numOfClusters.")
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
      # True labels
      plot(D$PC1, D$PC2, col = D$cluster, pch = 16, cex = 1.1,
           xlab = "Component 1", 
           ylab = "Component 2",
           main = "2D representation of the assigned clusters",
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

    getOutDf = function() {
      return(private$outDf)
    },
    
    getKmeansFit = function() {
      return(private$kmeansFit)
    }
  )
)

