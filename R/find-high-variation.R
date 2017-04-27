#' @export
calculateCOV <- function(column) {
  if (class(column) != 'numeric' && class(column) != 'integer') {
    stop('Your column must be of class numeric or integer')
  }
  
  mean <- mean(column, na.rm = TRUE)
  sd <- sd(column, na.rm = TRUE)
  COV <- (sd / mean) * 100
  COV
}

#' @export
searchForVariation <- function(df, 
                               categoricalCols, 
                               measureCols,
                               threshold) {
  # Notes: 
  # NA in output is bc there's only one value per group
  
  combineIndyVars <- paste(categoricalCols, collapse = " + ")
  finalFormula <- paste(measureCols, " ~ ", combineIndyVars, sep = "")
  
  df <- aggregate(as.formula(finalFormula),
                  data = df, 
                  FUN = calculateCOV)
  print(df)  
  
  df <- removeRowsWithNAInSpecCol(df, measureCols)
  print(df)
  
  # Grab only variation above threshold
  df <- df[df[[measureCols]] > threshold,]
  df
}