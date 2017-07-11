#' @title 
#' Calculate coefficient of variation
#' @description Find coefficient of variation by dividing vector standard 
#' deviation by the mean.
#' @param vector A vector of numbers
#' @return A scalar
#' @export
#' @references \url{http://healthcare.ai}
#' \url{https://en.wikipedia.org/wiki/Coefficient_of_variation}
#' @seealso \code{\link{healthcareai}}, \code{\link{findVariation}}
#' @examples
#' df <- data.frame(a = c(1,2,NA,NA),
#'                  b = c(100,300,200,150))
#' res1 <- calculateCOV(df$a)
#' res1
#' 
#' res2 <- calculateCOV(df$b)
#' res2
calculateCOV <- function(vector) {
  if (class(vector) != 'numeric' && class(vector) != 'integer') {
    stop('Your vector must be of class numeric or integer')
  }
  
  meanVariable <- base::mean(vector, na.rm = TRUE)
  sdVariable <- stats::sd(vector, na.rm = TRUE)
  COV <- base::round((sdVariable / meanVariable), 2)
  COV
}

#' @title
#' Find all possible unique combinations
#' @description For a given vector of, find all possible combinations of the
#' values. When calculating, if two groups contain the same values, they are
#' counted as the same if they only differ in terms of ordering.
#' @param vector A vector of strings or numbers.
#' @return A list of sub-lists. Each sub-list represents one possible 
#' combination.
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} \code{\link{findVariation}} 
#' \code{\link{createVarianceTallTable}}
#' @examples
#' vector <- c("LactateOrderHospital",
#'                   "LactateOrderProvSpecialtyDSC",
#'                   "LactateOrderProvNM")
#' res <- createAllCombinations(vector)
#' 
#' # Let's look at one possible combination
#' unlist(res[3])
#' 
#' # Look at all possible combinations
#' res
createAllCombinations <- function(vector) {
  listOfCatColumnCombinations = list()
  df <- expand.grid(replicate(length(vector), 0:1, simplify = FALSE))
  
  for (i in 2:nrow(df)) { # Don't use 1st (all false row) from expand.grid
    listOfCatColumnCombinations <- c(listOfCatColumnCombinations,
                                     list(vector[as.logical(df[i,])]))
  }
  listOfCatColumnCombinations
}

#' @title 
#' Transform a dataframe to be three columns and tall instead of wide
#' @description When dealing with a table that could be unexpectedly wide,
#' it helps to instead fix its width and let it get tall (which makes it easy
#' to insert into a pre-existing table). This assists the findVariation 
#' function.
#' @param df A dataframe
#' @param categoricalCols Vector of strings, representing categorical column(s)
#' @param measure String, representing measure column
#' @return A dataframe of eight columns. MeasureVolumeRaw denotes number of rows 
#' in the particular subgroup; MeasureVolumePercent denotes percent of rows in 
#' that subgroup as a percentage of the above subgroup (i.e., F within Gender);
#' MeasureImpact is the subgroup COV * VolRaw (i.e., num of rows).
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} 
#' \code{\link{findVariation}}
#' @examples
#' df <- data.frame(LactateOrderProvSpecDSC = c("Pulmonary Disease",
#'                                              "Family Medicine"),
#'                  LactateOrderProvNM = c("Hector Salamanca",
#'                                         "Gus Fring"),
#'                  COV = c(0.43,0.35),
#'                  VolumeRaw = c(2,3),
#'                  VolumePercent = c(0.32,0.78),
#'                  Impact = c(0.46,1.05),
#'                  AboveMeanCOVFLG = c('Y','N'),
#'                  AboveMeanVolumeFLG = c('N','Y'))
#' 
#' head(df)
#' 
#' categoricalCols <- c("LactateOrderProvSpecDSC","LactateOrderProvNM")
#' 
#' dfRes <- createVarianceTallTable(df = df, 
#'                                  categoricalCols = categoricalCols, 
#'                                  measure = "LOS")
#' head(dfRes)
createVarianceTallTable <- function(df, 
                                    categoricalCols,
                                    measure) {
  
  if (any(categoricalCols %in% measure)) {
    stop('Your measure cannot also be listed in categoricalCols')
  }
  
  measureColumnVect <- c("COV",
                         "VolumeRaw",
                         "VolumePercent",
                         "Impact",
                         "AboveMeanCOVFLG",
                         "AboveMeanVolumeFLG")
  
  if (!all(categoricalCols %in% names(df))) {
    stop('One of the categoricalCols is not in the df. See the example ',
          'at ?createVarianceTallTable')
  }
  
  if (!all(measureColumnVect %in% names(df))) {
    stop('One of the required columns is not in the df. See the example ',
         'at ?createVarianceTallTable')
  }
  
  if (class(df$COV) != "numeric" &&  
      class(df$COV) != "integer") {
    stop("Your COV column needs to be of class numeric or integer")
  }

  if (class(df$VolumePercent) != "numeric" &&  
      class(df$VolumePercent) != "integer") {
    stop("Your VolumePercent column needs to be of class numeric or integer")
  }
  
  if (class(df$Impact) != "numeric" &&  
      class(df$Impact) != "integer") {
    stop("Your Impact column needs to be of class numeric or integer")
  }
  
  # Categories can't be factor cols, as paste0 uses labels instead of values
  df[categoricalCols] <- lapply(df[categoricalCols], as.character)
  
  # Combine cat column names via pipe delimiters
  combineCatColNamesPipe <- paste0(categoricalCols, collapse = "|")
  
  # Create first column of final dataframe (composed of category col names)
  # This column will be the only one that's the same in each row
  DimensionalAttributes <- base::rep(combineCatColNamesPipe, nrow(df))
  
  # Order dataframe, based on MeasureImpact column (w/ highest at top)
  #df <- df[order(-df[[incomingMeasureColImpact]]),]
  
  # Initialize final col names for last three cols
  CategoriesGrouped <- vector()
  MeasureCOV <- vector()
  MeasureVolumeRaw <- vector()
  MeasureVolumePercent <- vector()
  MeasureImpact <- vector()
  AboveMeanCOVFLG <- vector()
  AboveMeanVolumeFLG <- vector()
  
  # Going row by row through input df, populate final df
  for (i in 1:nrow(df)) {
    # Create pipe-delimited list of categories that create this group
    CategoriesGrouped <- c(CategoriesGrouped, 
                           paste0(df[i,!(names(df) %in% measureColumnVect)],
                                  collapse = "|"))
    
    MeasureCOV <- c(MeasureCOV, 
                    paste0(measure, "|", df[i,"COV"]))
    
    MeasureVolumeRaw <- c(MeasureVolumeRaw, 
                       paste0(measure, "|", df[i,"VolumeRaw"]))
  
    MeasureVolumePercent <- c(MeasureVolumePercent, 
                           paste0(measure, "|", df[i,"VolumePercent"]))
  
    MeasureImpact <- c(MeasureImpact, 
                       paste0(measure, "|", df[i,"Impact"]))
    
    AboveMeanCOVFLG <- c(AboveMeanCOVFLG, 
                         paste0(measure, "|", df[i,"AboveMeanCOVFLG"]))
    
    AboveMeanVolumeFLG <- c(AboveMeanVolumeFLG, 
                            paste0(measure, "|", df[i,"AboveMeanVolumeFLG"]))
    
  }
  
  dfResult <- data.frame(DimensionalAttributes, 
                         CategoriesGrouped, 
                         MeasureCOV,
                         MeasureVolumeRaw,
                         MeasureVolumePercent,
                         MeasureImpact,
                         AboveMeanCOVFLG,
                         AboveMeanVolumeFLG,
                         stringsAsFactors = FALSE)

  dfResult
}

#' @title
#' Find high variation
#' @description Search across subgroups and surface those that have coefficient
#' of variation * volume above a particular threshold
#' @param df A dataframe
#' @param categoricalCols Vector of strings representing categorical column(s)
#' @param measureColumn Vector of strings representing measure column(s)
#' @param dateCol Optional. A date(time) column to group by (done by month) 
#' @param threshold A scalar number, representing the minimum impact values
#' that are returned
#' @return A dataframe of eight columns. MeasureVolumeRaw denotes number of rows 
#' in the particular subgroup; MeasureVolumePercent denotes percent of rows in 
#' that subgroup as a percentage of the above subgroup (i.e., F within Gender);
#' MeasureImpact is the subgroup COV * VolRaw (i.e., num of rows).
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} \code{\link{calculateCOV}} 
#' \code{\link{createVarianceTallTable}}
#' @examples
#' df <- data.frame(Dept = c('A','A','A','B','B','B','B','B'),
#'                  Gender = c('F','M','M','M','M','F','F','F'),
#'                  LOS = c(3.2,NA,5,1.3,2.4,4,9,10))
#'
#' categoricalCols <- c("Dept","Gender")
#' 
#' dfRes <- findVariation(df = df, 
#'                        categoricalCols = categoricalCols,
#'                        measureColumn = "LOS")
#'
#' dfRes
findVariation <- function(df, 
                          categoricalCols,
                          measureColumn,
                          dateCol = NULL,
                          threshold = NULL) {
  
  if (!all(c(categoricalCols,measureColumn,dateCol) %in% names(df))) {
    stop('The measure column or one of the categorical cols is not in the df')
  }
  
  # Check that measure columns exist and are of proper type
  if (length(measureColumn) > 1) {
    for (i in 1:length(measureColumn)) {
      if (class(df[[measureColumn[i]]]) != "numeric" &&  
          class(df[[measureColumn[i]]]) != "integer") {
      stop("measureColumn needs to be of class numeric or integer. ",
           measureColumn[i], " appears to be of type ", 
           class(df[[measureColumn[i]]]))
      }
    }
  } else if (length(measureColumn) == 1) {
    if (class(df[[measureColumn]]) != "numeric" &&  
        class(df[[measureColumn]]) != "integer") {
      stop("measureColumn needs to be of class numeric or integer. ",
           measureColumn, " appears to be of type ", 
           class(df[[measureColumn]]))
    }
  }

  # Check that categorical columns exist and are of proper type
  if (length(categoricalCols) > 1) {
    for (i in 1:length(categoricalCols)) {
      if (class(df[[categoricalCols[i]]]) == "numeric" ||  
          class(df[[categoricalCols[i]]]) == "integer") {
        stop("categoricalCols cannot be of class numeric or integer. ", 
             categoricalCols[i], " appears to be of type ", 
             class(df[[categoricalCols[i]]]))
      }
    }
  } else if (length(categoricalCols) == 1) {
    if (class(df[[categoricalCols]]) == "numeric" ||  
        class(df[[categoricalCols]]) == "integer") {
      stop("categoricalCols cannot be of class numeric or integer. ", 
           categoricalCols, " appears to be of type ", 
           class(df[[categoricalCols]]))
    }
  }
  
  if (!is.null(dateCol)) {
    tryCatch(
      df[[dateCol]] <- as.Date(df[[dateCol]]),
      error = function(e) {
      e$message <- paste0(dateCol, " may not be a datetime column,",
                          " or the column may not be in format YYYY-MM-DD\n", e)
      stop(e)
    })

    # Convert date into YYYY-MM
    df[[dateCol]] <- base::format(df[[dateCol]],"%Y-%m")
    
    # Add dateCol to categoricalList (now that it's just YYYY-MM)
    categoricalCols <- c(categoricalCols, dateCol)
  }
  
  dfTotal <- data.frame()
  listOfPossibleCombos <- createAllCombinations(categoricalCols)
  
  for (i in 1:length(listOfPossibleCombos)) {
    
    for (j in 1:length(measureColumn)) {
    
      currentCatColumnComboVect <- unlist(listOfPossibleCombos[i])
      
      # Prepare for aggregate - Collapse cat cols into one str, separated by +
      combineIndyVarsPlus <- paste0(currentCatColumnComboVect, collapse = " + ")
      finalFormula <- paste0(measureColumn[j], " ~ ", combineIndyVarsPlus)
      
      dfSub <- stats::aggregate(stats::as.formula(finalFormula),
                                data = df,
                                FUN = function(x) 
                                  c(COV = healthcareai::calculateCOV(x), 
                                    VolumeRaw = NROW(x)))
      
      # Pull matrix that came out of aggregate and make them two regular columns
      dfSub$COV <- dfSub[[measureColumn[j]]][,'COV']
      dfSub$VolumeRaw <- dfSub[[measureColumn[j]]][,'VolumeRaw']
      dfSub[[measureColumn[j]]] <- NULL
      
      # Add on FLAGS for above-mean COV and VolumeRaw (at this level)
      dfSub$AboveMeanCOVFLG <- ifelse(dfSub$COV > mean(dfSub$COV, na.rm = TRUE), 
                                      'Y', 
                                      'N')
      
      dfSub$AboveMeanVolumeFLG <- ifelse(dfSub$VolumeRaw > mean(dfSub$VolumeRaw, 
                                                                na.rm = TRUE), 
                                        'Y', 
                                        'N')
      
      # Remove rows where COV is NA (which are due to only one row in subset)
      dfSub <- healthcareai::removeRowsWithNAInSpecCol(dfSub, "COV")
      
      # Create percentages for Volume
      dfSub$VolumePercent <- round(dfSub$VolumeRaw / sum(dfSub$VolumeRaw), 2)
      
      # Create total impact column
      dfSub$Impact <- dfSub$COV * dfSub$VolumeRaw
      
      # Select only impact above threshold
      if (!is.null(threshold)) {
        dfSub <- dfSub[dfSub$Impact > threshold,]
      }
  
      # If no rows in subgroup above threshold, send to next loop
      if (isTRUE(all(is.na(dfSub))) || nrow(dfSub) == 0) {
        next
      }
      
      # Create pipe-delimited, fixed number of columns and add to overall df
      dfTotal <- 
        rbind(dfTotal,
              healthcareai::createVarianceTallTable(
                              df = dfSub, 
                              categoricalCols = currentCatColumnComboVect,
                              measure = measureColumn[j]))
    }
  }

  if (nrow(dfTotal) == 0) {
    stop("No subgroups found above threshold.",
         " Try removing or lower your threshold, or select more rows")
  } else {
    
    # Convert from factor to char for OVERALL ordering
    #dfTotal$DimensionalAttributes <- as.character(dfTotal$DimensionalAttributes)
    
    # Prepare for OVERALL ordering by parsing piped strings
    # Unlist is necessary to create dataframe column
    dfTotal$TempDimDepth <- unlist(lapply(dfTotal$DimensionalAttributes, 
                                          FUN = getPipedWordCount))
    dfTotal$TempImpact <- unlist(lapply(dfTotal$MeasureImpact,
                                        FUN = getPipedValue))
    
    # Order dataframe, based on MeasureImpact column (w/ highest at top)
    dfTotal <- dfTotal[order(dfTotal$TempDimDepth,
                             dfTotal$DimensionalAttributes,
                             -dfTotal$TempImpact),]
    
    dfTotal$TempDimDepth <- NULL
    dfTotal$TempImpact <- NULL
    
    return(dfTotal)
  }
}

#' @title
#' Count number of words in pipe-delimited string
#' @description 
#' For a given string with pipe(s), count the number of word-like sections
#' that are separated by pipes.
#' @param string A string with pipes
#' @return A count of number of words in input string.
#' @export
#' @seealso \code{\link{healthcareai}} \code{\link{findVariation}} 
#' \code{\link{createVarianceTallTable}}
#' @examples
#' res <- getPipedWordCount('hello|sir')
#' res
getPipedWordCount <- function(string) {
  result <- length(unlist(strsplit(as.character(string), 
                                   split = "|", 
                                   fixed = TRUE)))
  result
}

#' @title
#' Grab number after single pipe in pipe-delimited string
#' @description
#' For a given string with a pipe, return the number that comes after the pipe
#' @param string A string with a pipe
#' @return A number from the original string
#' @export
#' @seealso \code{\link{healthcareai}} \code{\link{findVariation}} 
#' \code{\link{createVarianceTallTable}}
#' @examples
#' res <- getPipedValue('hello|23')
#' res
getPipedValue <- function(string) {
  result <- as.numeric(unlist(strsplit(as.character(string), 
                                       split = "|", 
                                       fixed = TRUE))[2])
  if (is.na(result)) {
    stop("Your input string doesn't contain either a |, a number, or both")
  } else {
    result
  }
}


#' @title
#' Find variation across groups
#' @description 
#' @import ggplot2
#' @param df A dataframe
#' @param categoricalCols Vector of strings representing categorical column(s)
#' @param measureColumn Vector of strings representing measure column(s)
#' @param dateCol Optional. A date(time) column to group by (done by month) 
#' @return A boxplot to compare variation across groups.
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} \code{\link{calculateCOV}} 
#' \code{\link{createVarianceTallTable}}
#' @examples
#' df <- data.frame(Dept = c('A','A','A','B','B','B','B','B'),
#'                  Gender = c('F','M','M','M','M','F','F','F'),
#'                  LOS = c(3.2,NA,5,1.3,2.4,4,9,10))
#'
#' categoricalCols <- c("Dept","Gender")
#' 
#' variationAcrossGroups(df = df, 
#'                        categoricalCols = categoricalCols,
#'                        measureColumn = "LOS")
#'


variationAcrossGroups <- function(df, 
                                  categoricalCols,
                                  measureColumn,
                                  dateCol = NULL) {
  
  if (!all(c(categoricalCols,measureColumn,dateCol) %in% names(df))) {
    stop('The measure column or one of the categorical cols is not in the df')
  }
  
  # Check that measure columns exist and are of proper type
  if (length(measureColumn) > 1) {
    for (i in 1:length(measureColumn)) {
      if (class(df[[measureColumn[i]]]) != "numeric" &&  
          class(df[[measureColumn[i]]]) != "integer") {
        stop("measureColumn needs to be of class numeric or integer. ",
             measureColumn[i], " appears to be of type ", 
             class(df[[measureColumn[i]]]))
      }
    }
  } else if (length(measureColumn) == 1) {
    if (class(df[[measureColumn]]) != "numeric" &&  
        class(df[[measureColumn]]) != "integer") {
      stop("measureColumn needs to be of class numeric or integer. ",
           measureColumn, " appears to be of type ", 
           class(df[[measureColumn]]))
    }
  }
  
  # Check that categorical columns exist and are of proper type
  if (length(categoricalCols) > 1) {
    for (i in 1:length(categoricalCols)) {
      if (class(df[[categoricalCols[i]]]) == "numeric" ||  
          class(df[[categoricalCols[i]]]) == "integer") {
        stop("categoricalCols cannot be of class numeric or integer. ", 
             categoricalCols[i], " appears to be of type ", 
             class(df[[categoricalCols[i]]]))
      }
    }
  } else if (length(categoricalCols) == 1) {
    if (class(df[[categoricalCols]]) == "numeric" ||  
        class(df[[categoricalCols]]) == "integer") {
      stop("categoricalCols cannot be of class numeric or integer. ", 
           categoricalCols, " appears to be of type ", 
           class(df[[categoricalCols]]))
    }
  }
  
  if (!is.null(dateCol)) {
    tryCatch(
      df[[dateCol]] <- as.Date(df[[dateCol]]),
      error = function(e) {
        e$message <- paste0(dateCol, " may not be a datetime column,",
                            " or the column may not be in format YYYY-MM-DD\n", e)
        stop(e)
      })
    
    # Convert date into YYYY-MM
    df[[dateCol]] <- base::format(df[[dateCol]],"%Y-%m")
    
    # Add dateCol to categoricalList (now that it's just YYYY-MM)
    categoricalCols <- c(categoricalCols, dateCol)
  }
  
  plot_list <- list()
  if (length(categoricalCols) == 1) {
    for (i in 1:length(measureColumn)) {
      p <- ggplot(aes(y = df[[measureColumn[i]]], x = df[[categoricalCols[1]]]), 
                  data = df) + geom_boxplot()
      p <- p + labs(x = categoricalCols[1], y = measureColumn[i])
      plot_list[[i]] <- p
    }
  } else if (length(categoricalCols) == 2) {
    for (i in 1:length(measureColumn)) {
      p <- ggplot(aes(y = df[[measureColumn[i]]], x = df[[categoricalCols[1]]],
                      fill = df[[categoricalCols[2]]]), data = df) + geom_boxplot()
      p <- p + scale_fill_discrete(categoricalCols[2]) + labs(x = categoricalCols[1], y = measureColumn[i])
      plot_list[[i]] <- p
    }
  } else if (length(categoricalCols) >= 3) {
    l <- list()
    for (i in 1:length(categoricalCols)) {
      l[[i]] <- df[[categoricalCols[i]]]
    }
    for (i in 1:length(measureColumn)) {
      p <- ggplot(aes(y = df[[measureColumn[i]]], x = interaction(l)), data = df) + geom_boxplot()
      p <- p + labs(y = measureColumn[i])
      plot_list[[i]] <- p
    }
  }
  
  for (i in 1:length(measureColumn)) {
    print(plot_list[[i]])
  }
  
}