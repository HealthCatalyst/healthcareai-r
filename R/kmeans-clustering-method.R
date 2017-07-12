#' Compare predictive models, created on your data
#'
#' @description This step allows you to create a random forest model, based on
#' your data.
#' @docType class
#' @usage ...
#' @import cluster
#' @importFrom R6 R6Class
#' @import ranger
#' @param object of UnsuperviseModelParameters class for $new() constructor
#' @param type The type of model (either 'regression' or 'classification')
#' @param df Dataframe whose columns are used for calc.
#' @param grainCol Optional. The dataframe's column that has IDs pertaining to 
#' the grain. No ID columns are truly needed for this step.
#' @param predictedCol Column that you want to predict. If you're doing
#' classification then this should be Y/N.
#' @param impute Set all-column imputation to F or T.
#' This uses mean replacement for numeric columns
#' and most frequent for factorized columns.
#' F leads to removal of rows containing NULLs.
#' @param debug Provides the user extended output to the console, in order
#' to monitor the calculations throughout. Use T or F.
#' @references \url{http://hctools.org/}
#' @seealso \code{\link{LassoDevelopment}}
#' @seealso \code{\link{LinearMixedModelDevelopment}}
#' @seealso \code{\link{healthcareai}}
#' @examples
#'
#' @export

KmeansClustering <- R6Class("KmeansClustering",
                            
  # Inheritance
  inherit = UnsupervisedModel,
  
  # Private members
  private = list(
    # Get kmeans model
    kmeans.fit = NA
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
    buildClusters = function() {
      # Standarize the variables
      df.stand <- scale(self$params$df)
      # K-Means
      private$kmeans.fit <- kmeans(df.stand, self$params$numOfClusters) 
      print(private$kmeans.fit)
      # attributes(k.means.fit)
      # Centroids:
      # k.means.fit$centers
      # Clusters:
      # k.means.fit$cluster
      # Cluster size:
      # k.means.fit$size
    },

    # Generate the plot to choose the best k
    elbow_plot = function(){
      mydata <- self$params$df
      wss <- (nrow(mydata) - 1)*sum(apply(mydata,2,var))
      for (i in 2:15) wss[i] <- sum(kmeans(scale(mydata),
                                           centers = i)$withinss)
      plot(1:15, wss, type = "b", xlab = "Number of Clusters",
           ylab = "Within groups sum of squares")
    },
    
    # Override: run k-means algorithm
    run = function() {
      
      # Start default logit (for row-wise var importance)
      # can be replaced with LIME-like functionality
      #private$fitGeneralizedLinearModel()
      
      # Build Model
      self$buildClusters()
      
      # self$elbow_plot()
      
      # save model
      #private$saveModel()
      
      # Perform prediction
      #self$performPrediction()
      
      # Generate performance metrics
      #self$generatePerformanceMetrics()
    }
    
  )
)

