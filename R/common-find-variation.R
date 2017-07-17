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
#' @description Plot a boxplot to compare the variation across the groups. 
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
#' df <- data.frame(Gender = factor(rbinom(100, 1, 0.45), labels = c("Male","Female")), 
#'                  Age = factor(rbinom(100, 1, 0.45), labels = c("Young","Old")),
#'                  Ans = factor(rbinom(100, 1, 0.45), labels = c("Y","N")),
#'                  Dept = sample(c("A","B","C"), 50, replace = TRUE, prob = c(0.2,0.3,0.5)),
#'                  LOS1 = c(rnorm(30),rnorm(70,10,5)),
#'                  LOS2 = c(rnorm(20,8,3),rnorm(80,-15,5)))
#'
#' categoricalCols <- c("Dept","Gender")
#' measureColumn <- c("LOS1","LOS2")
#' 
#' variationAcrossGroups(df = df, 
#'                        categoricalCols = categoricalCols,
#'                        measureColumn = measureColumn)
#'                        
#' treatment = c(rep("A", 20) , rep("B", 20) , rep("C", 20), rep("D", 20) ,  rep("E", 20))
#' value = c( sample(2:5, 20 , replace = TRUE) , sample(6:10, 20 , replace = TRUE), 
#'         sample(1:7, 20 , replace = TRUE), sample(3:10, 20 , replace = TRUE) , 
#'         sample(10:20, 20 , replace = TRUE) )
#' df1 = data.frame(treatment,value)
#' 
#' variationAcrossGroups(df1, categoricalCols = "treatment", measureColumn = "value")
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
  
  # Check if there are two many interactions
  if (length(categoricalCols) > 10) {
    stop("There are two many interactions. Length of categoricalCols cannot larger than 10.")
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
  
  # Function from package "multcompView": Convert a vector of hyphenated names into 
  # a character matrix with 2 columns containing the names split in each row
  vec2mat2 <- function(x, sep = "-") {
      splits <- strsplit(x, sep)
      n.spl <- sapply(splits, length)
      if (any(n.spl != 2)) 
        stop("Names must contain exactly one '", sep, "' each;  instead got ", 
             paste(x, collapse = ", "))
      x2 <- t(as.matrix(as.data.frame(splits)))
      dimnames(x2) <- list(x, NULL)
      x2
  }
  
  # Group the levels that are not different each other together
  generate_label_df <- function(TUKEY, variable){
    
    # Function from package "multcompView": Convert a vector of p-values
    # into a character based display in which common characters identify levels 
    # or groups that are not significantly different.
    multcompLetters <- function(x, compare = "<",
                                threshold = 0.05, Letters = c(letters, LETTERS, "."),
                                reversed = FALSE){
      ##
      ## 1.  Covert to logical
      ##
      x.is <- deparse(substitute(x))
      if (class(x) == "dist") x <- as.matrix(x)  
      if (!is.logical(x))
        x <- do.call(compare, list(x, threshold))
      ##
      ## 2.  Create array of distinct pairs
      
      dimx <- dim(x)
      {
        if ((length(dimx) == 2) && (dimx[1] == dimx[2])) {
          Lvls <- dimnames(x)[[1]]
          if (length(Lvls) != dimx[1])
            stop("Names requred for ", x.is)
          else{
            #       Create a matrix with 2 columns
            #       with the names of all pairs         
            x2. <- t(outer(Lvls, Lvls, paste,
                           sep = ""))
            x2.n <- outer(Lvls, Lvls,
                          function(x1, x2)nchar(x2))
            x2.2 <- x2.[lower.tri(x2.)]
            x2.2n <- x2.n[lower.tri(x2.n)]
            x2a <- substring(x2.2, 1, x2.2n)
            x2b <- substring(x2.2, x2.2n + 1)
            x2 <- cbind(x2a, x2b)
            x <- x[lower.tri(x)]        
          }
        }
        else{  
          namx <- names(x)
          if (length(namx) != length(x))
            stop("Names required for ", x.is)
          x2 <- vec2mat2(namx)
          Lvls <- unique(as.vector(x2))
        }
      }
      ##
      ## 3.  Find the names of the levels 
      ##  
      n <- length(Lvls)
      #   Generate an initial column
      LetMat <- array(TRUE, dim = c(n, 1),
                      dimnames = list(Lvls, NULL))
      ##
      ## 4.  How many distinct pairs?  
      ##  
      k2 <- sum(x)
      if (k2 == 0) {
        Ltrs <- rep(Letters[1], n)
        names(Ltrs) <- Lvls
        dimnames(LetMat)[[2]] <- Letters[1]
        return(list(Letters = Ltrs,
                    LetterMatrix = LetMat))  
      }
      ##
      ## 4.  At last 2 levels are different: 
      ##     insert & absorb
      ##  
      distinct.pairs <- x2[x,,drop = FALSE]
      absorb <- function(A.){
        #    Do the work in a recursive function:      
        #    Delete any column for which the TRUE 
        #    connections are a subset of another column
        k. <- dim(A.)[2]
        if (k. > 1) { #i. <- 1; j. <- 2
          for (i. in 1:(k. - 1)) for (j. in (i. + 1):k.) {
            if (all(A.[A.[, j.], i.])) {
              #### drop a redundant column and recurse ###
              A. <- A.[, -j., drop = FALSE]
              return(absorb(A.))
            }
            else {
              if (all(A.[A.[, i.], j.])) {
                #### drop a redundant column and recurse ###
                A. <- A.[, -i., drop = FALSE]
                return(absorb(A.))
              }
            }          
          }
        }
        #### end internal function absorb #######      
        A.
      }
      # Now apply this function 
      for (i in 1:k2) { # i <- 1+i
        #     Process the distinct differences one at a time       
        #     Insert    i <- 1+i
        #     Are (distinct) levels Td2[i, 1] and Td2[i,2]
        #        connected in any columns of A?
        dpi <- distinct.pairs[i,]
        ijCols <- (LetMat[dpi[1],] & LetMat[dpi[2], ])
        if (any(ijCols)) {
          #     "Insert":  Break this connection 
          A1 <- LetMat[, ijCols, drop = FALSE]
          A1[dpi[1],] <- FALSE
          LetMat[dpi[2], ijCols] <- FALSE
          LetMat <- cbind(LetMat, A1)
          #     Absorb   A. <- A
          LetMat <- absorb(LetMat)
        }
      }
      ##
      ## 5.  Sort the columns for visual appeal 
      ##  
      sortCols <- function(B){
        firstRow <- apply(B, 2, function(x)which(x)[1])
        B <- B[, order(firstRow)]
        #     If ties, sort submatrices
        firstRow <- apply(B, 2, function(x)which(x)[1])
        reps <- (diff(firstRow) == 0)
        if (any(reps)) {
          #     Break ties
          nrep <- table(which(reps))
          irep <- as.numeric(names(nrep))
          k <- dim(B)[1]
          for (i in irep) {
            i. <- i:(i + nrep[as.character(i)])
            j. <- (firstRow[i] + 1):k
            B[j., i.] <- sortCols(B[j., i., drop = FALSE])
          }
        }
        #### end internal function sortCols #######      
        B
      }
      LetMat. <- sortCols(LetMat)
      ### Should the letters go in the reversed order?
      if (reversed) LetMat. <- LetMat.[ ,rev(1:ncol(LetMat.))]
      # DON'T Sweep
      #...
      ##
      ## 6.  Create "Letters" for column names
      ##
      k.ltrs <- dim(LetMat.)[2]
      makeLtrs <- function(kl, ltrs=Letters){
        kL <- length(ltrs)
        if (kl < kL) return(ltrs[1:kl])
        ltrecurse <- c(paste(ltrs[kL], ltrs[-kL],
                             sep = ""), ltrs[kL])
        c(ltrs[-kL], makeLtrs(kl - kL + 1,
                              ltrecurse))
      }
      Ltrs <- makeLtrs(k.ltrs, Letters)
      dimnames(LetMat.)[[2]] <- Ltrs
      ##
      ## 7.  Create simple summaries
      ##
      LetVec <- rep(NA, n)
      names(LetVec) <- Lvls
      for (i in 1:n)
        LetVec[i] <- paste(Ltrs[LetMat.[i, ]],
                           collapse = "")
      nch.L <- nchar(Ltrs)
      # To allow for multicharacter "Letters", create
      # a vector of blanks with the right number
      # of characters for each.  
      blk.L <- rep(NA, k.ltrs)
      for (i in 1:k.ltrs)
        blk.L[i] <- paste(rep(" ", nch.L[i]), collapse = "")
      # Now create monospacedLetters:    
      monoVec <- rep(NA, n)
      names(monoVec) <- Lvls
      for (j in 1:n) {
        ch2 <- blk.L
        if (any(LetMat.[j,]))
          ch2[LetMat.[j,]] <- Ltrs[LetMat.[j,]]
        monoVec[j] <- paste(ch2, collapse = "")
      }
      ##
      ## 8.  done
      ##
      InsertAbsorb <- list(Letters = LetVec,
                           monospacedLetters = monoVec, 
                           LetterMatrix = LetMat.)
      class(InsertAbsorb) <- "multcompLetters"
      InsertAbsorb  
    }
    
    # Extract labels and factor levels from Tukey post-hoc 
    Tukey.levels <- TUKEY[[variable]][,4]
    if ((length(TUKEY[[variable]][,4])) == 1) 
      names(Tukey.levels) <- "L1-L2"
    Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
    
    #I need to put the labels in the same order as in the boxplot :
    Tukey.labels$treatment = rownames(Tukey.labels)
    Tukey.labels = Tukey.labels[order(Tukey.labels$treatment) , ]
    return(Tukey.labels)
  }
  
  l <- list()
  for (i in 1:length(categoricalCols)) {
    l[[i]] <- df[[categoricalCols[i]]]
  }

  # A panel of colors to draw each group with the same color :
    my_colors = c(rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255),
                   rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) ,
                   rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),
                   rgb(165,103,40,maxColorValue = 255))

  
  for (i in 1:length(measureColumn)) {
    model <- lm(df[[measureColumn[i]]] ~ interaction(l))
    ANOVA <- aov(model)
    # Tukey test to study each pair of treatment :
    TUKEY <- TukeyHSD(x = ANOVA, conf.level = 0.95)
    labs = generate_label_df(TUKEY, 'interaction(l)')

    par(bg = "transparent", cex.axis = 1)
    a <- boxplot(df[[measureColumn[i]]]~interaction(l), data = df, col = my_colors[as.numeric(labs[,1])],
            ylab = measureColumn[i], ylim = c(min(df[measureColumn[[i]]], na.rm = TRUE), 
                                              1.1*max(df[[measureColumn[i]]], na.rm = TRUE)),
            cex.lab = 1.25)
    # Now set the plot region to grey
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
    grid(nx = NULL, ny = NULL, lwd = 1, lty = 3, col = "white") #grid over boxplot
    par(new = TRUE)
    a <- boxplot(df[[measureColumn[i]]]~interaction(l), data = df, col = my_colors[as.numeric(labs[,1])],
            ylab = measureColumn[i], ylim = c(min(df[measureColumn[[i]]]) , 1.1*max(df[[measureColumn[i]]])),
            main = paste("Boxplot of", measureColumn[i], "Across", paste(categoricalCols,collapse = " ")),
            cex.lab = 1.25, outcol = "lightcoral", add = TRUE)
    over = 0.1*max(a$stats[nrow(a$stats),] )
    text(c(1:nlevels(interaction(l))) , a$stats[nrow(a$stats),] + over, labs[,1],
         col = my_colors[as.numeric(labs[,1])])
  }
  
  # Return tables with mean/std and quartiles
  resTable <- list()
  completeDf <- df[complete.cases(df),]
  
  compl <- list()
  for (i in 1:length(categoricalCols)) {
    compl[[i]] <- completeDf[[categoricalCols[i]]]
  }
  
  for (j in 1:length(measureColumn)) {
    completeDf$newcol <- interaction(compl)
    levels <- unique(completeDf$newcol)
    means <- c()
    std <- c()
    minVal <- c()
    firstQuartile <- c()
    m <- c()
    thirdQuartile <- c()
    maxVal <- c()
    for (i in 1:length(levels)) {
      means[i] <- mean(completeDf[completeDf$newcol == levels[i],][[measureColumn[j]]], na.rm = T)
      std[i] <- sd(completeDf[completeDf$newcol == levels[i],][[measureColumn[j]]], na.rm = T)
      minVal[i] <- min(completeDf[completeDf$newcol == levels[i],][[measureColumn[j]]], na.rm = T)
      firstQuartile[i] <- quantile(completeDf[completeDf$newcol == levels[i],][[measureColumn[j]]], 0.25, na.rm = T)
      m[i] <- median(completeDf[completeDf$newcol == levels[i],][[measureColumn[j]]], na.rm = T)
      thirdQuartile[i] <- quantile(completeDf[completeDf$newcol == levels[i],][[measureColumn[j]]], 0.75, na.rm = T)
      maxVal[i] <- max(completeDf[completeDf$newcol == levels[i],][[measureColumn[j]]], na.rm = T)
    }
    se <- means/std
    measures <- rep(measureColumn[j], length(levels))
    # Output the table
    resTable[[j]] <- data.frame(measure = measures, group = levels, Mean = means, 
                                Std = std, SE = se, Min = minVal, 
                      Q1 = firstQuartile, Median = m, Q3 = thirdQuartile, Max = maxVal)
  }
  
  tabRes <- data.frame()
  for (i in 1:length(measureColumn)) {
    tabRes <- rbind(tabRes,resTable[[i]])
  }
  
  return(format(tabRes, digits = 3))
}