# Round numeric columns in a data frame
#
# @param df data frame
# @param digits number of digits to round to; defaults to 2
#
# @return data frame
#
# @keywords internal
# @examples
# df <- data.frame(x = rnorm(5), y = letters[1:5])
# attr(df, "one") <- 3
# df
# attributes(df)
roundNumericCols <- function(df, digits = 2) {
  
  # Grab attributes to reapply before returning
  ats <- attributes(df)

  # Round numeric columns; return others unchanged
  df <- 
    lapply(df, function(column) {
    if (is.numeric(column))
      round(column, digits) else
        column
      })
  
  # Reapply attributes
  df <- as.data.frame(df)
  attributes(df) <- ats
  
  return(df)
}
