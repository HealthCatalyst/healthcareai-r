#' @title 
#' Calculate coefficient of variation
#' @description Find coefficient of variation by dividing vector standard 
#' deviation by the mean and muliplying by 100.
#' @param vector A vector of numbers
#' @return A scalar
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
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
  
  meanVar <- base::mean(vector, na.rm = TRUE)
  sdVar <- stats::sd(vector, na.rm = TRUE)
  COV <- (sdVar / meanVar) * 100
  base::round(COV, 2)
}

#' @title 
#' Transform a dataframe to be three columns and tall instead of wide
#' @description When dealing with a table that could be unexpectedly wide,
#' it helps to instead fix its width and let it get tall (which makes it easy
#' to insert into a pre-existing table). First use findVariation function.
#' @param df A dataframe
#' @param categoricalCols Vector of categorical column(s)
#' @param measureColumn A string. The measure of interest.
#' @return A dataframe of four columns
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} 
#' \code{\link{findVariation}}
#' @examples
#' df <- data.frame(LactateOrderProvSpecialtyDSC = c("Pulmonary Disease",
#'                                                   "Family Medicine"),
#'                  LactateOrderProvNM = c("Hector Salamanca",
#'                                         "Gus Fring"),
#'                  LOSCOV = c(43.4,35.1),
#'                  LOSVolume = c(2,3),
#'                  LOSImpact = c(86.8,105.3))
#' 
#' head(df)
#' 
#' categoricalCols <- c("LactateOrderProvSpecialtyDSC","LactateOrderProvNM")
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
  
  # Create measure-realted col names--these cols are not the same in each row
  incomingMeasureColCOV <- paste0(measure,'COV')
  incomingMeasureColVolume <- paste0(measure,'Volume')
  incomingMeasureColImpact <- paste0(measure,'Impact')
  
  measureColumnVect <- c(incomingMeasureColCOV,
                         incomingMeasureColVolume,
                         incomingMeasureColImpact)
  
  if (!all(c(categoricalCols,measureColumnVect) %in% names(df))) {
    stop('The measure column or one of the categorical cols is not in the df')
  }
  
  if (class(df[[incomingMeasureColCOV]]) != "numeric" &&  
      class(df[[incomingMeasureColCOV]]) != "integer") {
    stop("Your ", incomingMeasureColCOV, " column needs to be of class numeric",
         " or integer")
  }

  if (class(df[[incomingMeasureColVolume]]) != "numeric" &&  
      class(df[[incomingMeasureColVolume]]) != "integer") {
    stop("Your ", incomingMeasureColVolume, " column needs to be of class",
         " numeric or integer")
  }
  
  if (class(df[[incomingMeasureColImpact]]) != "numeric" &&  
      class(df[[incomingMeasureColImpact]]) != "integer") {
    stop("Your ", incomingMeasureColImpact, " column needs to be of class",
         " numeric or integer")
  }
  
  # Categories can't be factor cols, as paste0 uses labels instead of values
  df[categoricalCols] <- lapply(df[categoricalCols], as.character)
  
  # Combine cat column names via pipe delimiters
  combineCatColNamesPipe <- paste0(categoricalCols, collapse = "|")
  
  # Create first column of final dataframe (composed of category col names)
  # This column will be the only one that's the same in each row
  DimensionalAttributes <- base::rep(combineCatColNamesPipe, nrow(df))
  
  # Order dataframe, based on MeasureImpact column (w/ highest at top)
  df <- df[order(-df[[incomingMeasureColImpact]]),]
  
  # Initialize final col names for last three cols
  CategoriesGrouped <- vector()
  MeasureCOV <- vector()
  MeasureVol <- vector()
  MeasureImpact <- vector()
  
  # Going row by row through input df, populate final df
  for (i in 1:nrow(df)) {
    # Collapse non-measure cols
    # Don't include the metric columns when finding what the groupers are
    CategoriesGrouped <- c(CategoriesGrouped, 
                           paste0(df[i,!(names(df) %in% measureColumnVect)],
                                  collapse = "|"))
    
    MeasureCOV <- c(MeasureCOV, paste0(measure, 
                                       "|",
                                       df[i,incomingMeasureColCOV]))
    MeasureVol <- c(MeasureVol, paste0(measure,
                                       "|",
                                       df[i,incomingMeasureColVolume]))
    MeasureImpact <- c(MeasureImpact, paste0(measure,
                                             "|",
                                             df[i,incomingMeasureColImpact]))
  }
  
  dfResult <- data.frame(DimensionalAttributes, 
                         CategoriesGrouped, 
                         MeasureCOV,
                         MeasureVol,
                         MeasureImpact)
  dfResult
}

#' @title
#' Find high variation
#' @description Search across subgroups and surface those that have coefficient
#' of variation * volume above a particular threshold
#' @param df A dataframe
#' @param categoricalCols Vector of categorical columns
#' @param measureColumn A string of the measure column of interest
#' @param dateCol Optional. A date(time) column to group by (done by month) 
#' @param threshold A number, representing the minimum impact (which is 
#' coefficient of variation multiplied by the volume for that subgroup)
#' @return A dataframe of three columns
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
#'                        measureColumn = "LOS",
#'                        threshold = 50)
#'
#' dfRes

findVariation <- function(df, 
                          categoricalCols,
                          measureColumn,
                          dateCol = NULL,
                          threshold = NULL) {
  # TODO: Can measureColumn be a 0/1 col? Does that make sense?
  
  if (!all(c(categoricalCols,measureColumn,dateCol) %in% names(df))) {
    stop('The measure column or one of the categorical cols is not in the df')
  }
  
  if (class(df[[measureColumn]]) != "numeric" &&  
      class(df[[measureColumn]]) != "integer") {
    stop("Your ", measureColumn, " column needs to be of class numeric or integer")
  }
  
  for (i in 1:length(categoricalCols)) {
    if (class(df[[i]]) == "numeric" ||  
        class(df[[i]]) == "integer") {
      stop("categoricalCols cannot be of class numeric or integer. ", 
           categoricalCols[i], " appears to be of type ", class(df[[i]]))
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

    df[[dateCol]] <- base::format(df[[dateCol]],"%Y-%m")
    
    # Add dateCol to categoricalList (now that it's just YYYY-MM)
    categoricalCols <- c(categoricalCols, dateCol)
  }
  
  listOfPossibleCombos <- createCombinations(categoricalCols)
  dfTotal <- data.frame()
  
  for (i in 1:length(listOfPossibleCombos)) {
    
    currentCatColumnComboVect <- unlist(listOfPossibleCombos[i])
    
    # Collapse cat cols into one string, separated by +
    combineIndyVarsPlus <- paste0(currentCatColumnComboVect, collapse = " + ")
    finalFormula <- paste0(measureColumn, " ~ ", combineIndyVarsPlus)
    
    dfSub <- stats::aggregate(stats::as.formula(finalFormula),
                              data = df,
                              FUN = function(x) 
                                c(COV = healthcareai::calculateCOV(x), 
                                  Volume = NROW(x)))
    
    # Create new column names, based on measure col
    newCOVName <- paste0(measureColumn, 'COV')
    newVolName <- paste0(measureColumn, 'Volume')
    newImpactName <- paste0(measureColumn, 'Impact')
    
    # Pull matrix that came out of aggregate and make them two regular columns
    dfSub[[newCOVName]] <- dfSub$LOS[,'COV']
    dfSub[[newVolName]] <- dfSub$LOS[,'Volume']
    
    # Delete matrix column that came out of aggregate
    dfSub[[measureColumn]] <- NULL
    
    # Remove rows where there aren't more 
    dfSub <- healthcareai::removeRowsWithNAInSpecCol(dfSub, 
                                                     newCOVName)
    
    # Create total impact column
    dfSub[[newImpactName]] <- dfSub[[newCOVName]] * dfSub[[newVolName]]
    
    # Select only impact above threshold
    if (!is.null(threshold)) {
      
      dfSub <- dfSub[dfSub[[newImpactName]] > threshold,]
      
      # If no rows in subgroup above threshold, send to next loop
      if (isTRUE(all(is.na(dfSub)))) {
        next
      }
    }

    # Add variation/volumne for one categorical col combination to total df
    dfTotal <- 
      rbind(dfTotal,
            healthcareai::createVarianceTallTable(
                            df = dfSub, 
                            categoricalCols = currentCatColumnComboVect,
                            measure = measureColumn ))
  }
  
  if (nrow(dfTotal) == 0) {
    stop("No subgroups found above threshold.",
         " Try removing or lower your threshold")
  } else {
    return(dfTotal)
  }
}

#' @title
#' Find all possible combinations
#' @description For a given vector of strings, find all possible combinations 
#' of those strings. 
#' @param categoricalCols A vector of strings
#' @return A list of sub-lists. Each sub-list represents one possible 
#' combination.
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} \code{\link{findVariation}} 
#' \code{\link{createVarianceTallTable}}
#' @examples
#' categoricalCols <- c("LactateOrderHospital",
#'                      "LactateOrderProvSpecialtyDSC",
#'                      "LactateOrderProvNM")
#' y <- createCombinations(categoricalCols)
#' 
#' # Let's look at one possible combination
#' print(unlist(y[3]))


createCombinations <- function(categoricalCols) {
  listOfCatColumnCombinations = list()
  df <- expand.grid(replicate(length(categoricalCols), 0:1, simplify = FALSE))
  
  for (i in 2:nrow(df)) { # Don't use 1st (all false row) from expand.grid
    listOfCatColumnCombinations <- c(listOfCatColumnCombinations,
                                     list(categoricalCols[as.logical(df[i,])]))
  }
  listOfCatColumnCombinations
}