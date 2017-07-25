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
#' @param pca Optional. TRUE or FALSE. If TRUE, perform PCA on the raw data.
#' @param usePrinComp Optional. TRUE or FALSE. If TRUE, will use the principle components
#' to perform K-means clustering. Default is FALSE.
#' @param numOfPrinComp number of principle components you want to use to perdorm 
#' K-means clustering. Must be given if usePrinComp is TRUE.
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
#' @references \url{https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R}
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
    prin.comp = NA,
    df.prin = NA,
    prop_varex = NA,
    centers = NA,
    cluster = NA,
    outDf = NA,
    cluster.labels = NA,
    #labels = NA, # the label in the raw data set
    
    # Calculate the euclidean distance between two vectors
    euc.dist = function(x1, x2) {
      sqrt(sum((x1 - x2) ^ 2))
    },
    
    #check if the data type is numeric
    checkDataType = function() {
      #print(self$params$dataType)
      if (self$params$dataType != 'numeric' || isNumeric(self$params$df) == FALSE) {
        stop("Your data type must be numeric in order to use kmeans.")
      }
    },
    
    # Scale the data
    dataScale = function() {
      
    },
    
    # PCA analysis
    pcaAnalysis = function() {
      private$prin.comp <- prcomp(self$params$df, scale. = T)
      #compute standard deviation of each principal component
      std_dev <- private$prin.comp$sdev
      #compute variance
      pr_var <- std_dev^2
      #proportion of variance explained
      private$prop_varex <- pr_var/sum(pr_var)
    },
    
    
    # Extract the principle components 
    prinCompDf = function() {
      private$df.prin <- data.frame(private$prin.comp$x)
      private$df.prin <- private$df.prin[,1:self$params$numOfPrinComp]
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
      lineMag <- private$lineMagnitude(x1, y1, x2, y2)
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
        ix <- private$lineMagnitude(px, py, x1, y1)
        iy <- private$lineMagnitude(px, py, x2, y2)
        if (ix > iy)  ans <- iy
        else ans <- ix
      } else {
        ## Intersecting point is on the line, use the formula
        ix <- x1 + u * (x2 - x1)
        iy <- y1 + u * (y2 - y1)
        ans <- private$lineMagnitude(px, py, ix, iy)
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
      private$distancePointSegment(x,y, x1,y1, x2,y2)
    },
    
    
    ## The findElbow function was found at 
    ## https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R
    findElbow = function() {
      k.max <- 15 # Maximal number of clusters
      data <- scale(self$params$df)
      wss <- sapply(1:k.max, 
                    function(k){kmeans(data, k, nstart = 10 )$tot.withinss})
      # Find the elbow using the method described in
      # stackoverflow.com/a/2022348/633251
      # but translated to R (see above).
      
      # Add an index to argument values for easy plotting
      DF <- data.frame(x = 1:length(wss), y = wss)
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
      elbowd <- private$distancePointLine(DF$x[use], DF$y[use], coef(fit)[2], coef(fit)[1])
      DF$dist <- c(NA, elbowd, NA) # first & last points don't have a distance
      
      private$vnum <- which.max(DF$dist)
    }, # end of findElbow
    
    
    # Override: run k-means algorithm
    buildClusters = function() {
      
      ## Use principle components if usePrinComp is TRUE and the numOfPrinComp is given
      if (self$params$usePrinComp == TRUE && is.null(self$params$numOfPrinComp)) {
        stop("Principle components are not created because the numbder of principle components 
             is not dfined.")
      } else if (self$params$usePrinComp == TRUE && !is.null(self$params$numOfPrinComp)) {
        df <- private$prin.comp
      } else {
        df <- self$params$df
      }
      
      ## If the numOfClusters is not given, use the one generated by findElbow()
      if (!is.null(self$params$numOfClusters)) {
        numOfClusters <- self$params$numOfClusters 
      } else {
        numOfClusters <- private$vnum
      }
      
      #self$removeLabelCol()
      # Standarize the variables
      df.stand <- scale(df)
      # K-Means
      private$kmeans.fit <- kmeans(df.stand, numOfClusters)
      # Save the centers 
      private$centers <- private$kmeans.fit[["centers"]]
      private$cluster <- private$kmeans.fit[["cluster"]]
      #print(kmeans.fit)
      #return(invisible(private$kmeans.fit))
    },
    
    # Generate confusion matrix
    calculateConfusion = function() {
      # generate a confusion matrix of clusters and labels
      k.means.fit <- self$getKmeansfit()
      clusters <- k.means.fit$cluster
      #labels <- super$getLabelColVal()
      labels <- private$labelColValues
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
    
    # Assign labels to the clusters
    assignClusterLabels = function() {
      cm <- self$getConfusionMatrix()
      k <- nrow(self$getKmeansfit()[["centers"]])
      print(k)
      # take the cluster label from the highest percentage in that column
      private$cluster.labels <- list()
      for (i in 1:k) {
        private$cluster.labels <- rbind(private$cluster.labels, 
                                         row.names(cm)[match(max(cm[,i]), cm[,i])])
      }
      
      # this may still miss some labels, so make sure all labels are included
      for (l in rownames(cm)) { 
        if (!(l %in%  private$cluster.labels)) 
        { 
          cluster.number <- match(max(cm[l,]), cm[l,])
          private$cluster.labels[[cluster.number]] <- c(private$cluster.labels[[cluster.number]], l)
        } 
      }
    },
    
    # Label the new data points
    # getLabelOfNewdf = function(newdf) {
    #   # load newdf
    #   print(nrow(newdf))
    #   # compute euclidean distance from each sample to each cluster center
    #   labels <- c()
    #   for (i in 1:nrow(newdf)) {
    #     distToCenters <- c()
    #     for (j in 1:nrow(private$centers)) {
    #       distToCenters[j] <- private$euc.dist(newdf[i,],private$centers[j,])
    #     }
    #     labels[i] <- which.min(distToCenters)
    #   }
    #   return(labels)
    # },
    
    ## 
    createDf = function() {
      dtStamp <- as.POSIXlt(Sys.time())
      
      # Combine grain.col, prediction, and time to be put back into SAM table
      
      private$outDf <- data.frame(
        0,                                 # BindingID
        'R',                               # BindingNM
        dtStamp,                           # LastLoadDTS
        private$grainColValues,            # GrainID
        private$labelColValues,            # labelCol
        private$cluster)     # Top 3 Factor
      
      
      colnames(private$outDf) <- c(
        "BindingID",
        "BindingNM",
        "LastLoadDTS",
        self$params$grainCol,
        self$params$labelCol,
        "cluster"
      )
      
      # colNameOfoutDf <- names(private$outDf)
      # for (i in 1:ncol(private$outDf)) {
      #   if (nchar(colNameOfoutDf)[i] == 0) {
      #     private$outDf[[colNameOfoutDf[i]]] <- NULL
      #   }
      # }
      
      # Remove row names so df can be written to DB
      # TODO: in writeData function, find how to ignore row names
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
    
    getOutDf = function() {
      return(private$outDf)
    },


    # Override: run k-means algorithm
    run = function() {
      
      # Check data type
      private$checkDataType()
      
      # Check if pca is required
      if (self$params$pca == TRUE) {
        private$pcaAnalysis()
      }
      
      # check whether to use principle components or not
      if (!is.null(self$params$numOfPrinComp))
        private$prinCompDf()
      
      # Find elbow  
      private$findElbow()
      
      # Build Model
      private$buildClusters()
      
      # Plot the clusters
      # self$plotClusters()
      
      # Present confusion matrix
      private$calculateConfusion()
      
      # Assign labels to the clusters
      private$assignClusterLabels()
      
      # generate the df ready for output
      private$createDf()
      
      # save model
      #private$saveModel()
    
    },
    
    ## TODO: load data to make sure the newdf have the same format as df
    getLabelOfNewdf = function(x) {
      ## load data
      x <- scale(x)
      print(x)
      print(private$centers)
      # compute squared euclidean distance from each sample to each cluster center
      tmp <- sapply(seq_len(nrow(x)),
                    function(i) apply(private$centers, 1,
                                      function(v) sum((x[i, ] - v)^2)))
      max.col(-t(tmp))  # find index of min distance
    },
    
    
    #scree plot
    getScreePlot = function() {
      plot(private$prop_varex, xlab = "Principal Component",
           ylab = "Proportion of Variance Explained", type = "b", pch = 19)
    },
    
    # generawte elbow plot for k = 2 to k = 15
    getElbowPlot = function(df) {
      k.max <- 15 # Maximal number of clusters
      data <- scale(self$params$df)
      wss <- sapply(1:k.max, 
                    function(k){kmeans(data, k, nstart = 10 )$tot.withinss})
      plot(1:k.max, wss,
           type = "b", pch = 19, frame = FALSE, 
           xlab = "Number of clusters K",
           ylab = "Total within-clusters sum of squares")
      abline(v = private$vnum, lty = 2)
    },
    
    plotClusters = function() {
      df.stand <- scale(self$params$df)
      k.means.fit <- self$getKmeansfit()
      clusplot(df.stand, k.means.fit$cluster, main = '2D representation of the Cluster solution',
               color = TRUE, shade = TRUE, lines = 0)
    },
    
    # Plot Silhouette plot
    silhouettePlot = function() {
      dis <- dist(scale(self$params$df), method = "euclidean")
      k.means.fit <- self$getKmeansfit()
      plot(silhouette(k.means.fit$cluster,dis))
    },
    
    getKmeansfit = function() {
      return(private$kmeans.fit)
    },
    
    getConfusionMatrix = function() {
      return(private$confusionMatrix)
    },
    
    getClusterLabels = function() {
      return(private$cluster.labels)
    }
    
  )
)

