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
  meanVec <- lapply(df, mean, na.rm = TRUE)
  sdVec <- lapply(df, sd, na.rm = TRUE)
  r <- nrow(df)
  c <- ncol(df)
  scaledDf <- matrix(data = NA,nrow = r, ncol = c)
  scaledDf <- data.frame(scaledDf)
  names(scaledDf) <- names(df)
  for (i in 1:r) {
    scaledDf[i,] <- (df[i,] - meanVec)/sdVec
  }
  res <- list(means=meanVec, standard_deviations=sdVec, scaled_df=scaledDf)
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
  PCA <- prcomp(df, scale. = T)
  pcNum <- PCA$x
  #compute standard deviation of each principal component
  PcSd <- PCA$sdev
  #compute variance
  pcVar <- PcSd^2
  #proportion of variance explained
  propVarExplained <- pcVar/sum(pcVar)
  res <- list(PCs=data.frame(pcNum), prop_of_var=propVarExplained)
  return(res)
}

#' @title
#' Compute the distance between two points
#'
#' @description compute the distance between two points given the x, y axes of the
#' points.
#' @param x1 A numeric vector, x axis of the points
#' @param y1 A numeric vector, y axis of the points
#' @param x2 A numeric vector, x axis of the points
#' @param y2 A numeric vector, y axis of the points
#' @return A numeric vector, the length between two points.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' lineMagnitude(c(1,0),c(2,0),5,3)
lineMagnitude <- function(x1, y1, x2, y2) {
  dist <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(dist)
}

#' @title
#' Compute the distance of a point from a line segment 
#'
#' @description compute the distance of a point from a line segment given the 
#' x, y axes of the points.
#' @param px A numeric vector, x axis of the points
#' @param py A numeric vector, y axis of the points
#' @param x1 A number, x axis of the end point of the line segment
#' @param y1 A number, y axis of the end point of the line segment
#' @param x2 A number, x axis of the other end point of the line segment
#' @param y2 A number, y axis of the other end point of the line segment
#' @return  numeric vector, the length between the pointS and the line segment. If the 
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
#' distancePointSegment(c(2,0),c(3,2),1,2,3,1)
distancePointSegment <- function(px, py, x1, y1, x2, y2) {
  ans <- NULL
  ix <- iy <- 0   # intersecting point
  lineMag <- lineMagnitude(x1, y1, x2, y2)
  if (any(lineMag < 0.00000001)) { 
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
#' @param x A numeric vector, x axis of the points
#' @param y A numeric vector, y axis of the points
#' @param slope A number, the slope of the line
#' @param intercept A number, the intercept of the line 
#' @return A numeric vector, the length of the pointS from the line. 
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @references This helper funtion is originally found here 
#' \url{http://paulbourke.net/geometry/pointlineplane/pointline.r}
#' @references \url{https://github.com/bryanhanson/ChemoSpecMarkeR/blob/master/R/findElbow.R}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' distancePointLine(c(2,0),c(3,2),-0.5,2.5)
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
#' @param y A numeric vector of more than 2 elements  w/o NA's
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
  if (length(y) <= 2) 
    stop("The curve has less than 2 points. At least three points in a curve are needed to find its elbow.")
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
  if (!concave) warning("Your curve doesn't appear to be concave, using 2 PCs.")

  # Calculate the orthogonal distances
  use <- 2:(nrow(DF) - 1)
  elbowd <- distancePointLine(DF$x[use], DF$y[use], coef(fit)[2], coef(fit)[1])
  DF$dist <- c(NA, elbowd, NA) # first & last points don't have a distance

  vnum <- which.max(DF$dist)
  if (!concave) vnum <- 2
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
  return(as.data.frame(confusionMatrix))
}

#' @title
#' Assign labels to the kmeans confusion matrix 
#'
#' @description Finds the correct label for a the kmeans confusion matrix. This function
#' assumes that the most populated cluster is the correct one for a given label.
#' @param cm A square dataframe that holds a confusion matrix.
#' Rownames must correspond to correct labels.
#' @return A character vector of the label names.
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
#' assignClusterLabels(cm)
#' 
assignClusterLabels <- function(cm) {
    # Error check to make sure the number of clusters and labels are equal.
  if (ncol(cm)!=nrow(cm)){
    stop("If you are using a labeled dataset, you must use the same number of clusters and labels")
  }
  if (any(apply(cm, 1, duplicated, incomparables = 0))) {
    # If there are duplicates in the rows
    warning('The boundary between 2 clusters perfectly splits a known category. 
      This usually indicates a problem with the data. 
      Label assignments are not available.')
    clusterLabels <- colnames(cm)
  } else if (any(apply(cm, 2, duplicated, incomparables = 0))) {
    # If there are dupiclates in the columns
    warning('One known category is split evenly into two clusters.
      This usually indicates a problem with the data. 
      Label assignments are not available.')
    clusterLabels <- colnames(cm)
  } else {
    # Take the cluster label from the highest percentage in that row
    clusterLabels <- character(length = length(rownames(cm)))
    for (i in 1:length(rownames(cm))) {
      clusterLabels[which(cm[i,]==max(cm[i,]))] <- rownames(cm)[i]
    }
  }
  return(clusterLabels)
}
