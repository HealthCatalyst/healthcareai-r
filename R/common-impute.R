#' @title
#' Last observation carried forward
#'
#' @description
#' Carries the last observed value forward for all columns in a data.table
#' grouped by an id.
#'
#' @param df data frame sorted by an ID column and a time or sequence number
#' column.
#' @param id A column name (in ticks) in df to group rows by.
#' @return A data frame where the last non-NA values are carried forward
#' (overwriting NAs) until the group ID changes.
#'
#' @import data.table
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df <- data.frame(personID=c(1,1,2,2,3,3,3),
#'                 wt=c(.5,NA,NA,NA,.3,.7,NA),
#'                 ht=c(NA,1,3,NA,4,NA,NA),
#'                 date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
#'                        '01/01/2015','01/15/2015','01/30/2015'))
#'
#' head(df,n=7)
#'
#' dfResult <- groupedLOCF(df, 'personID')
#'
#' head(dfResult, n = 7)
groupedLOCF <- function(df, id) {
  # Note that the object that results acts as both a data frame and datatable
  df <- data.table::setDT(df)
  
  # Create a vector of booleans where each element is mapped to a row in the
  # data.table.  Each value is FALSE unless the corresponding row is the first row
  # of a person.  In other words, each TRUE represents a change of PersonID in the
  # data.table.
  changeFlags <- c(TRUE, get(id, df)[-1] != get(id, df)[-nrow(df)])
  
  # A helper that finds the last non-NA value for a given column x in df.
  locf <- function(x) x[cummax(((!is.na(x)) | changeFlags) * seq_len(nrow(df)))]
  
  # By avoiding using the 'by' operator of data.table, we're reducing the number of
  # calls from (N rows / P people) * C columns to just C columns; this is just once
  # for each column in the data.table.
  dfResult <- df[, lapply(.SD, locf)]
  dfResult
}

#' @title
#' Perform imputation on a dataframe
#'
#' @description This class performs imputation on a data frame. For numeric columns,
#' the column-mean is used; for factor columns, the most frequent value is used.
#' @param df A dataframe of values with NAs.
#' @param imputeVals A list of values to be used for imputation. If an unnamed list
#' must be the same length as as the number of columns in `df` and the order of values
#' in `imputeVals` should match the order of columns in `df`. If named, names will be
#' matched to names of `df`, and you can provide a subset of columns in `df` (see @details).
#' @return A list. The first element, df, is the imputed dataframe. The second element,
#' imputeVals, is a list of the imputation value used.
#' @details If `imputeVals` is a named list containing a subset of the columns in `df`,
#' columns that don't have a value provided will have one calculated. If you wish
#' to provide custom imputation values for a subset of columns and leave NAs
#' in other columns, supply the value NA to `imputeVals` for those columns.
#' #'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' 
#' # Impute a single column
#' df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
#'    c=c(11,21,31,43), d=c('Y','N','N',NA))
#' df <- df['a'] # note df[,1] does not return a df!
#' out <- imputeDF(df)
#' dfOut <- out$df # imputed data frame
#' imputeVals <- out$imputeVals # imputed values
#' print(dfOut)

#' # Impute an entire data frame
#' df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
#'    c=c(11,21,31,43), d=c('Y','N','N',NA))
#' out <- imputeDF(df)
#' dfOut <- out$df # imputed data frame
#' imputeVals <- out$imputeVals # imputed values
#' print(dfOut)
#' 
#' # To impute using your own values (one per column)
#' df <- data.frame(a=c(1,2,3,NA), b=c('Y','N','Y',NA),
#'    c=c(11,21,31,43), d=c('Y','N','N',NA))
#' myValues <- list(2, 'Y', 26.5, 'N') 
#' out <- imputeDF(df, myValues)
#' dfOut <- out$df # imputed data frame
#' print(dfOut)

imputeDF <- function(df, imputeVals = NULL) {
  # Use separate functions to make applying easier in deploy
  # This function finds the value to impute for a column
  getImputeValuesColumn <- function(col) {
    if (is.numeric(col)) {
      value <- mean(col, na.rm = TRUE) # this is where the method could change
    } else {
      value <- names(which.max(table(col)))
    }
    return(value)
  }
  
  # Check to make sure that df is a dataframe
  if (!(is.data.frame(df))) {
    stop('df must be a dataframe.')
  }
  
  # Check to make sure imputeVals is a list or NULL
  if (!(is.list(imputeVals) || is.null(imputeVals))) {
    stop("imputeValues must be a list.")
  }
  
  # Check to make sure imputeVals are either all named or all unnamed
  nameLengths <- sapply(names(imputeVals), nchar)  # Unnamed are actually character(0)
  if (!(all(nameLengths == 0) || all(nameLengths > 0)))
    stop("Either name all or none of imputeVals")
  
  # Remove any named entries in imputeVals that aren't in df, with a warning
  to_remove <- which(!names(imputeVals) %in% names(df))
  if (length(to_remove) > 0) {
    warning("Didn't find ", paste(names(imputeVals)[to_remove], collapse = " or "), " in df. Ignoring...")
    imputeVals <- imputeVals[-to_remove]
  }
  
  # If user provided imputeVals and they're unnamed they have to be length 0 or same as df
  if (!is.null(imputeVals) && is.null(names(imputeVals))) {
    # If it's length 0, make it NULL
    if (length(imputeVals) == 0) {
      imputeVals <- NULL
      # If it's same length as df, give it names of df
    } else if  (length(imputeVals) == ncol(df)) {
      # Check to make sure each is of the same type as the respective column.
      # Want characters and factors to align, likewise for integers, doubles, etc.
      # So do a little hacky regex
      char_imputeVals <- sapply(imputeVals, function(x) grepl("[a-zA-Z]", x))
      char_cols <- sapply(df, function(x) any(grepl("[a-zA-Z]", x)))
      if (!all.equal(char_imputeVals, unname(char_cols)))
        stop("You seem to have an impute value of a different type than the 
                corresponding column in the data frame. Consider naming the values 
                in imputeVals with the corresponding column in the data frame.")

      names(imputeVals) <- names(df)
    } else {
      stop("imputeVals must be named, have one entry for each column in df, or be left blank.")
    }
  }
  
  # Change factor columns to characters so that missing levels are dealt with correctly.
  for (col in names(df)[sapply(df, is.factor)]) {
    df[[col]] <- as.character(df[[col]])
  }
  
  # Identify columns that don't have imputation value provided by the user
  toImpute <- names(df)[!names(df) %in% names(imputeVals)]
  
  # Get impute values for columns that don't have a value
  imputeVals <- c(imputeVals, lapply(df[toImpute], getImputeValuesColumn))
  
  # Replace missing values with imputeVals
  for (varname in names(imputeVals))
    df[[varname]][is.na(df[[varname]])] <- imputeVals[[varname]]
  
  # Change characters back to factors.
  for (col in names(df)[sapply(df, is.character)]) {
    df[[col]] <- as.factor(df[[col]])
  }
  
  # Put imputeVals in order of df, just in case that matters downstream
  imputeVals <- imputeVals[match(names(df), names(imputeVals))]
  
  return(list(df=df, imputeVals=imputeVals))
}

#' @title
#' Depreciated in favor of \code{imputeDF}
#'
#' @description This class performs imputation on a vector. For numeric vectors
#' the vector-mean is used; for factor columns, the most frequent value is used.
#' @param v A vector, or column of values with NAs.
#' @return A vector, or column of values now with no NAs
imputeColumn <- function(v) {
  stop('This function was depreciated. Use imputeDF instead.')
}
