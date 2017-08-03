#' @title
#' Center and scale columns in a numeric data frame
#'
#' @description center and scale columns in a numeric data frame using means and 
#' standard deviations
#' @param df A numeric data frame
#' @return A list that contains a vector of column means, a vector of column
#' sds, and a scaled data frame.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(a = c(2,1,3,2,4),b = c(NA,8,6,7,9))
#' res <- dataScale(df)
#' res

dataScale <- function(df) {
  mean.vec <- apply(df, 2, mean, na.rm = TRUE)
  sd.vec <- apply(df, 2, sd, na.rm = TRUE)
  r <- nrow(df)
  c <- ncol(df)
  scaledf <- matrix(data = NA,nrow = r, ncol = c)
  scaledf <- data.frame(scaledf)
  names(scaledf) <- names(df)
  for (i in 1:r) {
    scaledf[i,] <- (df[i,] - mean.vec)/sd.vec
  }
  res <- list(mean.vec, sd.vec, scaledf)
  names(res) <- c("means", "standard_deviations", "scaled_df")
  return(res)
}

#' @title
#' Perform principle component analysis
#'
#' @description performs PCA on numeric data frames
#' @param df A numeric data frame w/o NA's
#' @return A list that contains the data frame of principle components and 
#' the proportion of variance explained by each PC.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' data(iris)
#' head(iris)
#' df <- iris[,1:4]
#' res <- pcaAnalysis(df)
#' head(res[[1]])

pcaAnalysis <- function(df) {
  if (sum(is.na(df)) > 0) stop("Missing values in data frame")
  prin.comp <- prcomp(df, scale. = T)
  pcs <- prin.comp$x
  #compute standard deviation of each principal component
  std_dev <- prin.comp$sdev
  #compute variance
  pr_var <- std_dev^2
  #proportion of variance explained
  prop_varex <- pr_var/sum(pr_var)
  res <- list(data.frame(pcs), prop_varex)
  names(res) <- c("PCs","prop_of_var")
  return(res)
}

#' @title
#' Compute the distance between two points
#'
#' @description compute the distance between two points given the x, y axes of the
#' points.
#' @param x1 A number, x axis of one point
#' @param y1 A number, y axis of one point
#' @param x2 A number, x axis of the other point
#' @param y2 A number, y axis of the other point
#' @return A number, the length between two points.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' lineMagnitude(1,2,5,3)
lineMagnitude <- function(x1, y1, x2, y2) {
  dist <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(dist)
}

#' @title
#' Compute the distance of a point from a line segment 
#'
#' @description compute the distance of a point from a line segment given the 
#' x, y axes of the points.
#' @param px A number, x axis of the point
#' @param py A number, y axis of the point
#' @param x1 A number, x axis of the end point of the line segment
#' @param y1 A number, y axis of the end point of the line segment
#' @param x2 A number, x axis of the other end point of the line segment
#' @param y2 A number, y axis of the other end point of the line segment
#' @return A number, the length between the point and the line segment. If the 
#' intersection point is outside the line segment, return the distance to the nearest 
#' endpoint.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @references The helper funtion is originally found here
#' \url{http://paulbourke.net/geometry/pointlineplane/pointline.r}
#' @references \url{https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' distancePointSegment(2,3,1,2,3,1)
distancePointSegment <- function(px, py, x1, y1, x2, y2) {
  ans <- NULL
  ix <- iy <- 0   # intersecting point
  lineMag <- lineMagnitude(x1, y1, x2, y2)
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
    ix <- lineMagnitude(px, py, x1, y1)
    iy <- lineMagnitude(px, py, x2, y2)
    if (ix > iy)  ans <- iy
    else ans <- ix
  } else {
    ## Intersecting point is on the line, use the formula
    ix <- x1 + u * (x2 - x1)
    iy <- y1 + u * (y2 - y1)
    ans <- lineMagnitude(px, py, ix, iy)
  }
  return(ans)
}

#' @title
#' Compute the distance of a point from a line 
#'
#' @description compute the distance of a point from a line given the 
#' x, y axes of the point and the slope and intercept of the line.
#' @param x A number, x axis of the point
#' @param y A number, y axis of the point
#' @param slope A number, the slope of the line
#' @param intercept A number, the intercept of the line 
#' @return A number, the length of the point from the line. 
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @references This helper funtion is originally found here 
#' \url{http://paulbourke.net/geometry/pointlineplane/pointline.r}
#' @references \url{https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' distancePointLine(2,3,-0.5,2.5)
distancePointLine <- function(x, y, slope, intercept) {
  x1 <- x - 10
  x2 <- x + 10
  y1 <- x1*slope + intercept
  y2 <- x2*slope + intercept
  ans <- distancePointSegment(x,y, x1,y1, x2,y2)
  return(ans)
}

#' @title
#' Find the elbow in a curve
#'
#' @description finds the elbow of a curve that is concave to the line connecting
#' the first and last points. 
#' @param y A numeric vector w/o NA's, values of the points in the curve
#' @return A number, the index of the elbow point. 
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @references This function is developed based on the following function in 
#' package ChemoSpecMarkeR
#' \url{https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R}
#' @references The idea behind the function can be found here
#' \url{https://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve/2022348#2022348}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' y <- c(8.5, 4.9, 2.8, 2.5, 1.9, 1.1, 1.1, 0.9)
#' plot(y) # concave
#' findElbow(y)
#' 
#' y <- c(6,5.5,4,2,1.5)
#' plot(y) # not concave
#' ## findElbow(y) will gave an error
findElbow <- function(y) {
  # Add an index to argument values for easy plotting
  DF <- data.frame(x = 1:length(y), y = y)
  fit <- lm(y ~ x, DF[c(1,nrow(DF)),]) # 2 point 'fit'
  m <- coef(fit)[2]
  b <- coef(fit)[1]

  # Check to make sure the data is concave
  concave <- FALSE
  use <- 2:(nrow(DF) - 1)
  refpts <- m*DF$x[use] + b
  if (all(refpts > DF$y[use]) | all(refpts < DF$y[use])) concave <- TRUE
  if (!concave) stop("Your curve doesn't appear to be concave")

  # Calculate the orthogonal distances
  use <- 2:(nrow(DF) - 1)
  elbowd <- distancePointLine(DF$x[use], DF$y[use], coef(fit)[2], coef(fit)[1])
  DF$dist <- c(NA, elbowd, NA) # first & last points don't have a distance

  vnum <- which.max(DF$dist)
  return(vnum)
} 

#' @title
#' Generate confusion matrix of percentages
#'
#' @description Generate confusion matrix and convert from raw counts to percentage
#' of each label
#' @param labels A vector with countable unique items, usually a factor variable 
#' in a data frame, labeling each observation.
#' @param clusters A vector with countable unique items, usually is the clustering 
#' results returned by kmeans(), No NA's.
#' @return A confusion matrix of percentages.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' data(iris)
#' head(iris)
#' kmeans.fit <- kmeans(iris[,1:4],3)
#' labs <- iris[,5]
#' cls <- kmeans.fit[["cluster"]]
#' calculateConfusion(labels = labs, clusters = cls)
#' 
calculateConfusion <- function(labels, clusters) {
  # Generate a confusion matrix of clusters and labels
  numOfLabels <- length(unique(labels)) - sum(is.na(unique(labels)))
  d <- data.frame(state = labels, cluster = clusters)
  td <- as.data.frame(table(d))
  # Convert from counts to percentages
  confusionMatrix <- matrix(ncol = max(clusters), nrow = 0) # k col
  for (i in 1:numOfLabels) {
    total <- sum(td[td$state == td$state[i],3])
    confusionMatrix <- rbind(confusionMatrix, td[td$state == td$state[i],3]/total)
  }
  rownames(confusionMatrix) <- td[1:numOfLabels,1]
  return(confusionMatrix)
}

#' @title
#' Assign labels to the clusters 
#'
#' @description compute the distance of a point from a line given the 
#' x, y axes of the point and the slope and intercept of the line.
#' @param cm A confusion matrix, with each row converting to percentage
#' @param k A number, number of clusters
#' @return A list of length k, showing the labels that are assigned to each cluster.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' data(iris)
#' head(iris)
#' kmeans.fit <- kmeans(iris[,1:4],3) 
#' labs <- iris[,5]
#' cls <- kmeans.fit[["cluster"]]
#' cm <- calculateConfusion(labels = labs, clusters = cls)
#' assignClusterLabels(cm, 3)
#' 
assignClusterLabels <- function(cm, k) {
  # Take the cluster label from the highest percentage in that column
  cluster.labels <- list()
  for (i in 1:k) {
    cluster.labels <- rbind(cluster.labels, row.names(cm)[match(max(cm[,i]), cm[,i])])
  }

  # Make sure all labels are included
  for (l in rownames(cm)) {
    if (!(l %in%  cluster.labels)) {
      cluster.number <- match(max(cm[l,]), cm[l,])
      cluster.labels[[cluster.number]] <- c(cluster.labels[[cluster.number]], l)
    }
  }
  return(cluster.labels)
}

#' @title
#' Extract Silhouette Information from Clustering 
#'
#' @description Compute silhouette information according to a given k means
#' clustering result
#' @param x A vector of integers, the clustering result returned by kmeans(), 
#' such as kmeans.fit$cluser
#' @param dist A dissimilarity object resurned by dist()
#' @return An n x 3 matrix with columns of cluster, neighbor and 
#' silhouette width
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @references silhouette() in package cluster
#' \url{https://cran.r-project.org/web/packages/cluster/index.html} 
#' @seealso \code{\link{healthcareai}}
#' @examples
#' data(iris)
#' head(iris)
#' kmeans.fit <- kmeans(scale(iris[,1:4]),3)
#' cluster <- kmeans.fit$cluster
#' dis <- dist(scale(iris[,1:4]))
#' res <- getSilhouetteInf(cluster, dis)
#' str(res)
#' 
getSilhouetteInf <- function(x, dist) {
  #cll <- match.call()
  n <- length(x)
  clusters <- sort(unique(x))
  k <- length(clusters)
  
  distMat <- as.matrix(dist)# so we can apply(.) below
  
  # Create the output matrix
  resMat <- matrix(data = NA, nrow = n, ncol = 3)
  colnames(resMat) <- c("cluster","neighbor","sil_width")
  
  for (j in 1:k) { # j-th cluster:
    idx <- x == clusters[j]
    sizeOfCluster <- sum(idx)
    resMat[idx, "cluster"] <- clusters[j]
    
    # Computer the mean distance of each point in cluster j to the other points in
    # other clsuters, get a matrix of size (k-1)*sizeOfClusterj
    distanceToOtherPts <- rbind(apply(distMat[!idx, idx, drop = FALSE], 2,
                                      function(a) tapply(a, x[!idx], mean)))
    
    # Get the row index with the minimal mean distance
    minimalDistIdx <- apply(distanceToOtherPts, 2, which.min)
    
    # Assign the cluster with minimal mean distance as the neighbour of cluster j
    resMat[idx,"neighbor"] <- clusters[-j][minimalDistIdx]
    
    # Calculate silhouette width 
    # https://en.wikipedia.org/wiki/Silhouette_(clustering)
    if (sizeOfCluster > 1) {
      # For each point i, compute the average distance of i to the other points 
      # in the same cluster: a(i)
      ai <- colSums(distMat[idx, idx])/(sizeOfCluster - 1) # length(a.i)= Nj
      # For each point i, find the minimal average distance of i to the other points
      # in the other cluters: b(i)
      bi <- distanceToOtherPts[cbind(minimalDistIdx, seq(along = minimalDistIdx))]
      # s(i) = (b(i)-a(i))/max{a(i),b(i)}
      sil_width <- ifelse(ai != bi, (bi - ai) / pmax(bi, ai), 0)
    } else {
      sil_width <- 0
    }
    resMat[idx,"sil_width"] <- sil_width
  }
  return(resMat)
} 

#' @title
#' Plot silhouette width
#'
#' @description Plot silhouette width given a matrix that stores the silhouette
#' information
#' @param x a matrix returned by getSilhouetteInf()
#' @param col colors for different clusters
#' @return a silhouette plot
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @references plot.silhouette() function in package cluster
#' \url{https://cran.r-project.org/web/packages/cluster/index.html} 
#' @seealso \code{\link{healthcareai}}
#' @examples
#' data(iris)
#' head(iris)
#' kmeans.fit <- kmeans(scale(iris[,1:4]),3)
#' cluster <- kmeans.fit$cluster
#' dis <- dist(scale(iris[,1:4]))
#' res <- getSilhouetteInf(cluster, dis)
#' plotSilhouette(res,col = c(1:3))
#' 
plotSilhouette <- function(x, col = "gray") {
  n <- nrow(x)
  cluster <- x[,"cluster"]
  silhouette_width <- x[, "sil_width"]
  avgSilhouetteWidthofClusters <- tapply(silhouette_width, cluster, mean)
  sizeOfCluster <- table(cluster)
  avgSilhouetteWidth <- mean(silhouette_width)
  
  orderedx <- x[order(cluster, -x[,"sil_width"]) , , drop = FALSE]
  
  ## plot barplot
  bars <- rev(orderedx[, "sil_width"])
  cls <- orderedx[,"cluster"]
  space <- c(0, rev(diff(cls)))
  space[space != 0] <- 1
  
  k <- length(sizeOfCluster) 
  # Colors
  if (length(col) == 1) {
    col <- col
  } else if (length(col) == k) {
    col <- rev(col[cls])
  } else {
    stop("The length of the color vector does not match the number of clusters")
  }
  
  y <- barplot(bars, space = space,  
               xlab = "Silhouette width",
               xlim = c(min(0, min(bars)), 1),
               horiz = TRUE, las = 1, mgp = c(2.5, 1, 0),
               col = col, border = NA)
  title(main = "Silhouette plot", 
        sub = paste("Average silhouette width : ", round(avgSilhouetteWidth, digits = 2)))
  mtext(paste("( n =", n,")"))
  mtext(substitute(k ~~ "clusters" ~~ C[j], list(k = k)), adj = 1)
  mtext(expression(paste(j," :  ", n[j]," | ", avg[i %in% Cj] ~~ s[i])),
        adj = 1.04, line = -1.2)
  y <- rev(y)
  
  for (j in 1:k) {
    yj <- mean(y[cls == j])
    text(1, yj,
         paste(j,":  ", sizeOfCluster[j]," | ",
               format(avgSilhouetteWidthofClusters[j], digits = 1, nsmall = 2)),
         xpd = NA, adj = 0.8)
  }
}

