#' Build clusters using kmeans()
#'
#' @description This step allows you to use kmeans() to build clusters, based on
#' your data.
#' @docType class
#' @usage ...
#' @importFrom R6 R6Class
#' @import ranger
#' @import cluster
#' @param object of UnsuperviseModelParameters class for $new() constructor
#' @param dataType The type of your data set('numeric' for all numeric data set,
#' 'categorical' for all categorical data set, and "mixed" for data set that has both
#' numeric and categorical variables)
#' @param df Dataframe whose columns are used for calc.
#' @param newdf Optional. Assign clusters to a new data set. (Currenty only available for kmeans)
#' @param grainCol Optional. The dataframe's column that has IDs pertaining to 
#' the grain. No ID columns are truly needed for this step.
#' @param labelCol Optional. 
#' @param numOfCluster Number of clusters you want to build. 
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
    kmeans.fit = NA,
    confusionMatrix = NA,
    vnum = NA,
    
    #check if the data type is numeric
    checkDataType = function() {
      #print(self$params$dataType)
      if (self$params$dataType != 'numeric' || isNumeric(self$params$df) == FALSE) {
        stop("Your data type must be numeric in order to use kmeans.")
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

    
    # The following helper functions were found at
    # paulbourke.net/geometry/pointlineplane/pointline.r
    # via the SO reference below.  The short segment check
    # was modified to permit vectorization.
    
    lineMagnitude = function(x1, y1, x2, y2) {
      sqrt((x2 - x1)^2 + (y2 - y1)^2)
    },
    
    
    distancePointSegment = function(px, py, x1, y1, x2, y2) {
      ## px,py is the point to test.
      ## x1,y1,x2,y2 is the line to check distance.
      ##
      ## Returns distance from the line, or if the intersecting point on the line nearest
      ## the point tested is outside the endpoints of the line, the distance to the
      ## nearest endpoint.
      ##
      ## Returns 9999 on 0 denominator conditions.
      
      ans <- NULL
      ix <- iy <- 0   # intersecting point
      lineMag <- self$lineMagnitude(x1, y1, x2, y2)
      if (any(lineMag < 0.00000001)) { # modified for vectorization by BAH
        #warning("short segment")
        #return(9999)
        warning("At least one line segment given by x1, y1, x2, y2 is very short.")
      }
      u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1)))
      u <- u / (lineMag * lineMag)
      if (any((u < 0.00001) || (u > 1))) { # BAH added any to vectorize
        ## closest point does not fall within the line segment, take the shorter distance
        ## to an endpoint
        ix <- self$lineMagnitude(px, py, x1, y1)
        iy <- self$lineMagnitude(px, py, x2, y2)
        if (ix > iy)  ans <- iy
        else ans <- ix
      } else {
        ## Intersecting point is on the line, use the formula
        ix <- x1 + u * (x2 - x1)
        iy <- y1 + u * (y2 - y1)
        ans <- self$lineMagnitude(px, py, ix, iy)
      }
      ans
    },
    
    distancePointLine = function(x, y, slope, intercept) {
      ## x, y is the point to test.
      ## slope, intercept is the line to check distance.
      ##
      ## Returns distance from the line.
      ##
      ## Returns 9999 on 0 denominator conditions.
      x1 <- x - 10
      x2 <- x + 10
      y1 <- x1*slope + intercept
      y2 <- x2*slope + intercept
      self$distancePointSegment(x,y, x1,y1, x2,y2)
    },
    
    
    ## The findElbow function was found at 
    ## https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R
    findElbow = function(y, plot = FALSE, returnIndex = TRUE) {
      
      # Find the elbow using the method described in
      # stackoverflow.com/a/2022348/633251
      # but translated to R (see above).
      
      # Add an index to argument values for easy plotting
      DF <- data.frame(x = 1:length(y), y = y)
      fit <- lm(y ~ x, DF[c(1,nrow(DF)),]) # 2 point 'fit'
      m <- coef(fit)[2]
      b <- coef(fit)[1]
      
      # Check to make sure the data is concave as described
      # in the documentation, as arbitrary trends could give
      # misleading answers.  The following approach simply
      # checks to make sure all values are either above or
      # below the reference line.  This allows the values
      # to vary quite a bit and still return an answer.
      
      concave <- FALSE
      use <- 2:(nrow(DF) - 1)
      refpts <- m*DF$x[use] + b
      if (all(refpts > DF$y[use]) | all(refpts < DF$y[use])) concave <- TRUE
      if (!concave) stop("Your curve doesn't appear to be concave")
      
      # Calculate the orthogonal distances
      use <- 2:(nrow(DF) - 1)
      elbowd <- self$distancePointLine(DF$x[use], DF$y[use], coef(fit)[2], coef(fit)[1])
      DF$dist <- c(NA, elbowd, NA) # first & last points don't have a distance
      
      if (plot) {
        edm <- which.max(DF$dist)
        plot(DF[,1:2], type = "b", xlab = "index", ylab = "y values",
             main = "Looking for the Elbow")
        segments(DF$x[1], DF$y[1],
                 DF$x[nrow(DF)], DF$y[nrow(DF)], col = "red")
        points(DF$x[edm], DF$y[edm], cex = 1.5, col = "red")	
        points(DF$x[edm], DF$y[edm], pch = 20)
      }
      
      if (returnIndex) return(which.max(DF$dist))
      if (!returnIndex) return(DF)
      
    }, # end of findElbow
    

    
    # generawte elbow plot for k = 2 to k = 15
    elbow_plot = function(df) {
      k.max <- 15 # Maximal number of clusters
      data <- scale(self$params$df)
      wss <- sapply(1:k.max, 
                    function(k){kmeans(data, k, nstart = 10 )$tot.withinss})
      plot(1:k.max, wss,
           type = "b", pch = 19, frame = FALSE, 
           xlab = "Number of clusters K",
           ylab = "Total within-clusters sum of squares")
      
      private$vnum <- self$findElbow(wss)
      abline(v = private$vnum, lty = 2)
    },
    
    
    # Generate the plot to choose the best k    
    # elbow_plot = function(){
    #   df.stand <- scale(self$params$df)
    #   wss <- (nrow(df.stand) - 1)*sum(apply(df.stand,2,var))
    #   for (i in 2:15) wss[i] <- sum(kmeans(df.stand,
    #                                        centers = i)$withinss)
    #   plot(1:15, wss, type = "b", xlab = "Number of Clusters",
    #        ylab = "Within groups sum of squares")
    # },    
    
    # Override: run k-means algorithm
    buildClusters = function() {
      if (!is.null(p$numOfClusters)) {
          self$params$numOfClusters <- p$numOfClusters
      } else {
        self$params$numOfClusters <- private$vnum
      }
      #self$removeLabelCol()
      # Standarize the variables
      df.stand <- scale(self$params$df)
      # K-Means
      private$kmeans.fit <- kmeans(df.stand, self$params$numOfClusters)
      #print(kmeans.fit)
      return(invisible(private$kmeans.fit))
    },

    getKmeansfit = function() {
      return(private$kmeans.fit)
    },
    
    
    plotClusters = function() {
      df.stand <- scale(self$params$df)
      k.means.fit <- self$getKmeansfit()
      clusplot(df.stand, k.means.fit$cluster, main = '2D representation of the Cluster solution',
             color = TRUE, shade = TRUE, lines = 0)
    },

    # Generate confusion matrix
    calculateConfusion = function() {
      # generate a confusion matrix of clusters and labels
      k.means.fit <- self$getKmeansfit()
      clusters <- k.means.fit$cluster
      labels <- super$getLabelColVal()
      numOfLabels <- length(unique(labels)) - sum(is.na(unique(labels)))
      d <- data.frame(state = labels, cluster = clusters)
      td <- as.data.frame(table(d))
      # convert from raw counts to percentage of each label
      private$confusionMatrix <- matrix(ncol = max(clusters), nrow = 0) # k col
      for (i in 1:numOfLabels) {
        total <- sum(td[td$state == td$state[i],3])
        private$confusionMatrix <- rbind(private$confusionMatrix, td[td$state == td$state[i],3]/total)
      }
      rownames(private$confusionMatrix) <- td[1:numOfLabels,1]
      return(invisible(private$confusionMatrix))
    },
    
    getConfusionMatrix = function() {
      return(private$confusionMatrix)
    },
    
    # Assign labels to the clusters
    assignClusterLabels = function() {
      cm <- self$getConfusionMatrix()
      k <- self$params$numOfClusters
      # take the cluster label from the highest percentage in that column
      cluster.labels <- list()
      for (i in 1:k) {
        cluster.labels <- rbind(cluster.labels, row.names(cm)[match(max(cm[,i]), cm[,i])])
      }
      
      # this may still miss some labels, so make sure all labels are included
      for (l in rownames(cm)) { 
        if (!(l %in% cluster.labels)) 
        { 
          cluster.number <- match(max(cm[l,]), cm[l,])
          cluster.labels[[cluster.number]] <- c(cluster.labels[[cluster.number]], l)
        } 
      }
      return(cluster.labels)
    },
    
    # Plot Silhouette plot
    silhouettePlot = function() {
      dis <- dist(scale(self$params$df), method = "euclidean")
      k.means.fit <- self$getKmeansfit()
      plot(silhouette(k.means.fit$cluster,dis))
    },


    # Override: run k-means algorithm
    run = function() {
      
      # Check data type
      private$checkDataType()
      
      # elbow plot 
      self$elbow_plot()
      
      # Build Model
      self$buildClusters()
      
      # Plot the clusters
      # self$plotClusters()
      
      # Present confusion matrix
      self$calculateConfusion()
      
      # self$elbow_plot()
      
      # save model
      #private$saveModel()
    
    }
    
  )
)

