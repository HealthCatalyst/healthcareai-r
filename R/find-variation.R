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
#' it helps to instead fix it at three columns and let it get tall. Since this 
#' provides fixed columns, it's easy to put in a database table.
#' @param df A dataframe
#' @param categoricalCols Vector of categorical columns
#' @param measureCol A string of the measure column of interest
#' @return A dataframe of three columns
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} 
#' \code{\link{findVariation}}
#' @examples
#' df <- data.frame(LactateOrderProvSpecialtyDSC = c("Pulmonary Disease",
#'                                                   "Family Medicine"),
#'                  LactateOrderProvNM = c("Hector Salamanca",
#'                                         "Gus Fring"),
#'                  LactateResultNBR = c(43.4,35.1))
#' 
#' head(df)
#' 
#' categoricalCols <- c("LactateOrderProvSpecialtyDSC","LactateOrderProvNM")
#' measureCol <- "LactateResultNBR"
#' dfResult <- createVarianceTallTable(df, categoricalCols, measureCol)
#' head(dfResult)

createVarianceTallTable <- function(df, categoricalCols, measureCol) {
  
  if (!all(c(categoricalCols,measureCol) %in% names(df))) {
    stop('The measure column or one of the categorical cols is not in the df')
  }
  
  if (any(categoricalCols %in% measureCol)) {
    stop('Your measureCol cannot also be listed in categoricalCols')
  }
  
  # Categories can't be factor cols, as paste0 uses labels instead of values
  df[categoricalCols] <- lapply(df[categoricalCols], as.character)
  
  if (class(df[[measureCol]]) != "numeric" &&  
      class(df[[measureCol]]) != "integer") {
    stop("Your ", measureCol, " column needs to be of class numeric or integer")
  }
  
  # Combine cat column names via pipe delimiters
  combineCatColNamesPipe <- paste0(categoricalCols, collapse = "|")
  
  # Create first column of final dataframe (composed of category col names)
  # This column will be the same in each row
  DimensionalAttributes <- base::rep(combineCatColNamesPipe, nrow(df))
  
  # Going row by row through input df, populate 2nd and 3rd cols of final df
  CategoriesGrouped <- vector()
  MeasureCOV <- vector()
  for (i in 1:nrow(df)) {
    CategoriesGrouped <- c(CategoriesGrouped, paste0(df[i,-ncol(df)], 
                                                     collapse = "|"))
    
    MeasureCOV <- c(MeasureCOV, paste0(measureCol,"COV|",
                                       df[i,measureCol]))
  }
  
  dfResult <- data.frame(DimensionalAttributes, CategoriesGrouped, MeasureCOV)
  dfResult
}

#' @title
#' Find high variation
#' @description Search across subgroups and surface those that have variation
#' above a particular threshold
#' @param df A dataframe
#' @param categoricalCols Vector of categorical columns
#' @param measureCol A string of the measure column of interest
#' @param dateCol Optional. A date(time) column to group by (done by month) 
#' @param threshold A number, representing the minimum coefficient of variance
#' (COV) to flag and surface
#' @return A dataframe of three columns
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} \code{\link{calculateCOV}} 
#' \code{\link{createVarianceTallTable}}
#' @examples
#' df <- data.frame(Dept = c('A','A','A','B','B','B','B'),
#'                  Gender = c('F','M','M','M','M','F','F'),
#'                  LOS = c(3.2,NA,5,1.3,2.4,4,9))
#'
#' categoricalCols <- c("Dept","Gender")
#' 
#' df <- findVariation(df = df, 
#'                     categoricalCols = categoricalCols,
#'                     measureCol = "LOS")
#'
#' # Only subcategories w/ more than one row (with non-NA measure) are eligible
#' df

findVariation <- function(df, 
                          categoricalCols,
                          measureCol,
                          dateCol = NULL,
                          threshold = NULL) {
  # Notes: 
  # Can measureCol be a 0/1 col? Does that make sense?
  
  if (!all(c(categoricalCols,measureCol,dateCol) %in% names(df))) {
    stop('The measure column or one of the categorical cols is not in the df')
  }
  
  if (class(df[[measureCol]]) != "numeric" &&  
      class(df[[measureCol]]) != "integer") {
    stop("Your ", measureCol, " column needs to be of class numeric or integer")
  }
  
  # TODO: Check that categorical columns aren't classified as numbers
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
  
  # Collapse cat cols into one string, separated by +
  combineIndyVarsPlus <- paste0(categoricalCols, collapse = " + ")
  finalFormula <- paste0(measureCol, " ~ ", combineIndyVarsPlus)
  print('form')
  print(finalFormula)
  
  dfRes <- stats::aggregate(stats::as.formula(finalFormula),
                         data = df,
                         FUN = healthcareai::calculateCOV)
  
  print('hello')
  print(dfRes)
  df <- healthcareai::removeRowsWithNAInSpecCol(dfRes, measureCol)
  
  # Select only variation above threshold
  if (!is.null(threshold)) {
    dfRes <- dfRes[dfRes[[measureCol]] > threshold,]
  }
  
  dfRes <- healthcareai::createVarianceTallTable(dfRes, 
                                                 categoricalCols,
                                                 measureCol)
  dfRes
  
}