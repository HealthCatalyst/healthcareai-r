#' @title
#' Plot PR Curves from SupervisedModel classes
#'
#' @description Plot PRCurves calculated by children classes of SupervisedModel
#' @param PRCurves A vector/array/list of PR curves
#' @param names A vector of algorithm/class names
#' @param legendLoc Location of the legend string to display
#'
#' @importFrom graphics legend title
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
plotPRCurve <- function(PRCurves, names, legendLoc) {
  # generate color vector TODO: auto generate these colors dynamically as rgb
  # values based on the length of PRCurves list
  colvec <- c("red", "blue", "green", "orange", "brown", "magenta")
  
  # plot PRCurves
  prIndex <- 1
  for (pr in PRCurves) {
    if (prIndex == 1) {
      par(pty = "s")
      plot(x = pr@x.values[[1]],
           y = pr@y.values[[1]],
           col = colvec[prIndex],
           mar = c(4, 4, 3, 2) + 0.1,
           type = 'l',
           main = "PR Curve",
           xlab = "Recall", ylab = "Precision",
           # set axis limits manually
           xlim = c(0, 1), ylim = c(0, 1))
    } else {
      par(pty = "s")
      par(new = TRUE) # lay second line over first
      plot(x = pr@x.values[[1]],
           y = pr@y.values[[1]],
           col = colvec[prIndex], 
           lty = 2,
           type = 'l',
           main = "PR Curve",
           xlab = "Recall", ylab = "Precision",
           yaxt = "n", # turn off extra y axis
           # set axis limits manually
           xlim = c(0, 1), ylim = c(0, 1))
    }
    prIndex <- prIndex + 1
  }
  # legend
  legend(x = legendLoc, names, cex = 0.8, col = colvec, lty = 1:2, inset = 0.1)
  return()
}

#' @title
#' Plot ROCs from SupervisedModel classes
#'
#' @description Plot ROCs calculated by children classes of SupervisedModel
#' @param rocs A vector/array/list of ROC values
#' @param names A vector of algorithm/class names
#' @param legendLoc Location of the legend string to display
#'
#' @importFrom graphics legend title par
#' @importFrom utils head
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
plotROCs <- function(rocs, names, legendLoc) {
  # generate color vector TODO: auto generate these colors dynamically as rgb
  # values based on the length of rocs list
  colvec <- c("red", "blue", "green", "orange", "brown", "magenta")
  
  # plot ROCs
  rocIndex <- 1
  for (roc in rocs) {
    if (rocIndex == 1) {
      par(pty = "s")
      plot(x = roc@x.values[[1]],
           y = roc@y.values[[1]],
           col = colvec[rocIndex],
           mar = c(4, 4, 3, 2) + 0.1,
           type = 'l',
           main = "ROC",
           xlab = "False Positive Rate", ylab = "True Positive Rate")
      
    } else {
      par(pty = "s")
      par(new = TRUE) # lay second line over first
      plot(x = roc@x.values[[1]],
           y = roc@y.values[[1]], 
           col = colvec[rocIndex], 
           lty = 2,
           type = 'l',
           main = "ROC",
           xlab = "False Positive Rate", ylab = "True Positive Rate",
           yaxt = "n") # turn off extra y axis
    }
    rocIndex <- rocIndex + 1
  }
  # legend
  legend(x=legendLoc, names, cex = 0.8, col = colvec, lty = 1:2, inset = 0.1)
  return()
}