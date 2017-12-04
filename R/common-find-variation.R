#' @title 
#' Calculate coefficient of variation
#' @description Find coefficient of variation by dividing vector standard 
#' deviation by the mean.
#' @param vector A vector of numbers
#' @return A scalar
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
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
#' @references \url{http://healthcareai-r.readthedocs.io}
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
#' @references \url{http://healthcareai-r.readthedocs.io}
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
#' @param wideOutput If TRUE (default) categoricalCols and measureColumn will
#' be mixed in with results. If FALSE, output table will have Group and
#' Measure columns that reflect categoricalCols and measureColumn, and results
#' will appear in their own columns.
#' @return A dataframe of eight columns. MeasureVolumeRaw denotes number of rows 
#' in the particular subgroup; MeasureVolumePercent denotes percent of rows in 
#' that subgroup as a percentage of the above subgroup (i.e., F within Gender);
#' MeasureImpact is the subgroup COV * VolRaw (i.e., num of rows).
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
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
                          threshold = NULL,
                          wideOutput = TRUE) {
  
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
      
      if (wideOutput) {
      # Create pipe-delimited, fixed number of columns and add to overall df
      dfTotal <- 
        rbind(dfTotal,
              healthcareai::createVarianceTallTable(
                df = dfSub, 
                categoricalCols = currentCatColumnComboVect,
                measure = measureColumn[j]))
      } else {
        # Add measure column
        dfSub$Measure <- measureColumn[j]
        
        # Convet group column/s to Variable.Value
        if (length(listOfPossibleCombos[[i]]) == 1) {
          dfSub$Group <- paste(names(dfSub)[1], dfSub[, 1], sep = ".")
        } else {
          labs <- 
            sapply(listOfPossibleCombos[[i]], function(var) {
              column <- which(names(dfSub) == var)
              paste(names(dfSub)[column], dfSub[, column], sep = ".")
            })
          dfSub$Group <- apply(labs, 1, paste, collapse = "|")
        }
        # Get rid of old group/s column
        dfSub <- dfSub[, -which(names(dfSub) %in% listOfPossibleCombos[[i]])]
        # Move new group column to front
        dfSub <- dfSub[, c(ncol(dfSub):(ncol(dfSub) - 1), seq_len(ncol(dfSub) - 2))]
        dfTotal <- rbind(dfTotal, dfSub)
      }
    }
  }
  
  if (nrow(dfTotal) == 0) {
    stop("No subgroups found above threshold.",
         " Try removing or lower your threshold, or select more rows")
  } else {
    
    if (!wideOutput)
      return(dfTotal[order(-dfTotal$Impact), ])

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


#' @title Find variation across groups
#' @description Compare variation among groups on a continuous measure. Plot
#'   boxplot and perform ANOVA with Tukey's HSD statistical test to compare
#'   variation in a numeric variable (measureColumn) across groups
#'   (categoricalCols).
#'   
#' @param df A data frame containing group and measure columns.
#' @param categoricalCols Character. Vector containing the name(s) of column(s)
#'   to group by.
#' @param measureColumn Character. The name of the numeric variable of interest.
#' @param plotBoxplot Logical. Print boxplot with Tukey HSD labels?
#' @param plotGroupDifferences Optional. Logical. Plot results of Tukey's HSD
#'   test: mean differences between groups and confidence intervals for each
#'   pairwise group comparison? Default is FALSE.
#' @param returnGroupStats Optional. Logical. In addition to the model summary
#'   table, return summary statistics for each group? Default is FALSE
#' @param dateCol Optional. Date. A date(time) column to group by (grouped by
#'   month by default).
#' @param levelOfDateGroup Optional. Character. Level at which to group dateCol.
#'   One of "yearly", "quarterly", "monthly" (default), or "weekly".
#' @param sigLevel Optional. Numeric value between zero and one giving the alpha
#'   value for Tukey HSD test, i.e. the p-value threshold for significance.
#'   
#' @return By default, a data frame giving summary statistics from Tukey's HSD 
#'   test. If returnGroupStats is TRUE, a list of two data frames giving model
#'   and group summary statistics respectively.
#'   
#'   This function always induces the side effect of printing a boxplot to
#'   compare variation across groups. If plotGroupDifferences is TRUE, also
#'   plots a mean differences and confidence intervals between groups.
#'
#' @importFrom grDevices dev.off   
#' @importFrom grDevices png
#' @importFrom stats na.omit
#' @export
#' 
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}} \code{\link{findVariation}}
#' @references This function uses multcompView::multcompLetters() 
#'   \url{https://CRAN.R-project.org/package=multcompView}
#'   
#' @examples
#' ### Example 1 ###
#' 
#' # This example dataset has two columns: a blood test result (value)
#' # and an anesthetic treatment (anesthetic). There are five anesthetics in the 
#' # dataset: Midazolam, Propofol, Ketamine, Thiamylal, and Diazepam.
#' 
#' set.seed(35)
#' df1 <- data.frame(
#'   anesthetic = c(rep("Midazolam", 50), rep("Propofol", 20), rep("Ketamine", 40), 
#'                  rep("Thiamylal", 80),  rep("Diazepam", 20)),
#'   value = c(sample(2:5, 50, replace = TRUE), sample(6:10, 20, replace = TRUE), 
#'             sample(1:7, 40, replace = TRUE), sample(3:10, 80, replace = TRUE), 
#'             sample(10:20, 20, replace = TRUE)))                      
#' head(df1)
#' 
#' variationAcrossGroups(df1, "anesthetic", "value", sigLevel = .01)
#' 
#' # The boxplot tells us that Diazepam, Propofol, and Thiamylal all have 
#' # significantly different mean values from all other groups, including each other
#' # (p <= 0.01). Midazolam and Ketamine do not have significantly different mean 
#' # values because they share the label "a", but they are significantly different 
#' # from all the other treatments.
#' 
#' 
#' 
#' ### Example 2 ###
#' 
#' # This example dataset has three columns: department, admission date, and
#' # blood pressure reading (BP). We will examine whether blood pressures
#' # vary by department and year of admission.
#' 
#' set.seed(2017)
#' n <- 200
#' bp <- data.frame(department = sample(c("Cardiology", "Oncology", "Gastroenterology"), 
#'                                      size = n, 
#'                                      replace = TRUE, 
#'                                      prob =  c(0.5, 0.3, 0.2)),
#'                  admit_date = sample(seq(as.Date("2015-01-01"), 
#'                                          as.Date("2017-12-31"), 
#'                                          by = "day"), 
#'                                      size = n))
#' bp$BP <- floor(rnorm(n, 
#'                      120 * 
#'                        ifelse(bp$admit_date > "2015-12-31", 1.5, 1) +
#'                        ifelse(bp$department == "Cardiology", 80, 0),
#'                      ifelse(bp$department == "Oncology", 60, 30)))
#' head(bp)
#' 
#' variationAcrossGroups(bp, 
#'                       categoricalCols = "department", 
#'                       measureColumn = "BP", 
#'                       dateCol = "admit_date", 
#'                       levelOfDateGroup = "yearly",
#'                       plotGroupDifferences = TRUE)
#'                        
#' # Since plotGroupDifferences = TRUE and the default of returnGroupStats is
#' # FALSE, the function prints the boxplot and the 95% family-wise confidence
#' # interval plot, and returns the summary statistics data frame. The two plots
#' # show: 
#' # 
#' #   1. The boxplot of BP across all combinations of the two categories. 
#' #   department has 3 levels, as does date grouped by year, so there are a total
#' #   of 3 x 3 = 9 groupings, which are shown on the x axis of the boxplot. Groups
#' #   that have a shared letter are *not* significantly different. For example,
#' #   (Gastroenterology | 2015) and (Oncology | 2015) share a "b" label, so
#' #   patients in those groups do not have significantly different mean BP. On the
#' #   other hand, (Cardiology | 2015) and (Gastroenterology | 2015) do not share a
#' #   label, so patients in those groups do have significantly different BP. Likewise,
#' #   Oncology patients in 2015 have different BPs from Oncology patients in either of the subsequent
#' #   years, but Oncology patients in 2016 and 2017 do not have significantly
#' #   different BP (as shown by their shared "c" label). 
#' #   
#' #   2. Confidence-interval level plot This plot present the results
#' #   of the Tukey's Honest Significant Differences test. It compares all possible
#' #   pairs of groups and adjusts p-values for multiple comparisons. Red lines
#' #   indicate a significant difference between the two groups at the chosen 
#' #   significance level (0.05 by default). Groups are ordered by p-values. The
#' #   group with the greater mean value is always listed first (e.g. Cardiology |
#' #   2016 has greater BP than Oncology 2015).

variationAcrossGroups <- function(df, 
                                  categoricalCols,
                                  measureColumn,
                                  plotBoxplot = TRUE,
                                  plotGroupDifferences = FALSE,
                                  returnGroupStats = FALSE,
                                  dateCol = NULL,
                                  levelOfDateGroup = "monthly",
                                  sigLevel = .05) {
  
  if (!all(c(categoricalCols,measureColumn,dateCol) %in% names(df))) {
    stop('The measure column or one of the categorical cols is not in the df')
  }
  
  # Check that measure column exists and is of proper type
  if (length(measureColumn) > 1) {
    stop("You can only specify one measure column. If you want to examine
         variation over multiple numeric variables, call the function once 
         for each variable.")
  } else if (!is.numeric(df[[measureColumn]])) {
    stop("measureColumn needs to be of class numeric or integer. ",
         measureColumn, " appears to be of type ", 
         class(df[[measureColumn]]))
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
  
  # Remove rows with missingness in any of the variables that matter
  df <- df[, c(categoricalCols, measureColumn, dateCol)]
  df <- stats::na.omit(df)
  
  if (findInterval(1, 0:1, rightmost.closed = TRUE) != 1)
    stop("sigLevel must be between zero and one.")
  
  # Check if there are two many interactions
  if (length(categoricalCols) > 10) {
    stop("Check if there are two many interactions. Length of categoricalCols cannot be larger than 10.")
  } else if (length(categoricalCols) > 3) {
    warning(length(categoricalCols), " categorical variables is a lot of interactions to examine simultaniously. Consider looking at a subset of variables at a given time.")
  }
  
  if (!is.null(dateCol)) {
    tryCatch(
      df[[dateCol]] <- as.Date(df[[dateCol]]),
      error = function(e) {
        e$message <- paste0(dateCol, " may not be a datetime column,",
                            " or the column may not be in format YYYY-MM-DD\n", e)
        stop(e)
      })
    
    # Break date into groups
    if (levelOfDateGroup == "yearly") {
      # Yearly: convert date into YYYY
      df[[dateCol]] <- base::format(df[[dateCol]],"%Y")
    } else if (levelOfDateGroup == "quarterly") {
      # Quarterly: convert date into YYYY-MM
      df[[dateCol]] <- paste(base::format(df[[dateCol]], "%Y/"), 0, 
                             sub( "Q", "", quarters(df[[dateCol]], abbreviate = TRUE)), 
                             sep = "")
    } else if (levelOfDateGroup == "monthly") {
      # Monthly: convert date into YYYY-MM
      df[[dateCol]] <- base::format(df[[dateCol]],"%Y/%m")
    } else if (levelOfDateGroup == "weekly") {
      # Weekly:
      df[[dateCol]] <- paste(base::format(df[[dateCol]], "%Y/"), 
                             strftime(df[[dateCol]], format = "%W"), 
                             sep = "")
    } else {
      stop("levelOfDateGroup must be yearly, quarterly, monthly or weekly")
    }
    
    # Add dateCol to categoricalList (now that it's just YYYY-MM)
    categoricalCols <- c(categoricalCols, dateCol)
  }
  
  ## The case that there is only one categorical variable with 2 levels
  if (length(categoricalCols) == 1 &&
      length(unique(df[[categoricalCols]])) - sum(is.na(unique(df[[categoricalCols]]))) == 2) {
    vec <- levels(df[[categoricalCols]])
    vec <- vec[!is.na(vec)]
    name_spec <- paste(vec, collapse = "-")
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
                                threshold = sigLevel, Letters = c(letters, LETTERS, "."),
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
      names(Tukey.levels) <- name_spec
    Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
    
    # Put the labels in the same order as in the boxplot :
    Tukey.labels$groups = rownames(Tukey.labels)
    Tukey.labels = Tukey.labels[order(Tukey.labels$groups) , ]
    return(Tukey.labels)
  }
  
  l <- list()
  for (i in 1:length(categoricalCols)) {
    l[[i]] <- as.character(df[[categoricalCols[i]]])
  }
  
  
  model <- stats::lm(df[[measureColumn]] ~ interaction(l))
  ANOVA <- stats::aov(model)
  # Tukey test to examine each pair of groups :
  TUKEY <- stats::TukeyHSD(x = ANOVA, conf.level = 1 - sigLevel, ordered = TRUE)
  labs <- generate_label_df(TUKEY, 'interaction(l)')
  if (nrow(labs) > 2) {
    labs <- labs[levels(interaction(l)),]
  }
  # Hotfix for unknown missingness
  labs <- na.omit(labs)
  
  df$l <- sapply(1:length(l[[1]]), function(i) 
    paste(sapply(seq_len(length(l)), function(j) l[[j]][i]), collapse = ".")
  )
  
  # Make ggplot
  ggp <- 
    ggplot2::ggplot(df, ggplot2::aes_string(y = measureColumn)) + 
    ggplot2::geom_boxplot(ggplot2::aes(x = l))
  ggdata <- ggplot2::layer_data(ggp)
  xmove <- with(ggdata, mean(xmax - x)) / 2
  labels <- data.frame(
    x = ggdata$x + xmove,
    y = ggdata$upper,
    lab = labs$Letters
  )
  ggp <- 
    ggp +
    ggplot2::geom_label(ggplot2::aes(x = x, y = y, label = lab), labels) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                       hjust = 1, vjust = .5))
  if (plotBoxplot)
    print(ggp)
  
  # If plotGroupDifferences, write it to file
  
  
  if (plotGroupDifferences == TRUE) {		
    png("group_differences.png", height = 600, width = 800)
    # Sort group differences matrix to plot in order of ascending p-value		
    if (nrow(TUKEY[[1]]) > 1)		
      TUKEY[[1]] <- TUKEY[[1]][order(TUKEY[[1]][ , 4], -TUKEY[[1]][ , 1]), ]		
    tLabs <- gsub("\\.", "|", rev(rownames(TUKEY[[1]])))		
    lMar <- max(6, max(nchar(tLabs)) / 2)		
    lCEX <- 		
      if (length(tLabs) < 15) 1 else 		
        if (length(tLabs) < 25) .85 else 		
          if (length(tLabs) < 50) .7 else .6		
    
    # Different colors for signicantly different groups vs. not		
    cols <- ifelse(TUKEY[[1]][ , 4] <= sigLevel, "red", "black")		
    # graphics::frame()		
    graphics::par(mar = c(4.2, lMar, 3.8, 2))		
    graphics::plot(TUKEY, col = cols, yaxt = "n")		
    graphics::mtext(paste(measureColumn, "across", paste(categoricalCols, collapse = ", ")))		
    graphics::axis(2,		
                   at = seq_len(nrow(TUKEY[[1]])),		
                   labels = tLabs,		
                   las = 1,		
                   cex.axis = lCEX)
    dev.off()
  }
  
  
  

  # Create tables with pvalue for each pair of groups
  # Get 95% family-wise confidence level
  pvalueDF <- as.data.frame(TUKEY[[1]])
  pvalueDF$Groups <- rownames(pvalueDF)
  rownames(pvalueDF) <- NULL
  # Arrange this data frame
  pvalueDF <- pvalueDF[order(pvalueDF$`p adj`), 
                       which(names(pvalueDF) %in% c("Groups", "diff", "p adj"))]
  names(pvalueDF)[1:2] <- c("Mean Difference", "Adjusted p-value")
  pvalueDF <- pvalueDF[, c(3, 1, 2)]
  pvalueDF <- roundNumericCols(pvalueDF, digits = 3)
  rownames(pvalueDF) <- seq_len(nrow(pvalueDF))
  
  if (!returnGroupStats) {
    return(pvalueDF)
    
  } else {
    
    # Calculate summary stats on groups
    resTable <- list()
    completeDf <- df[stats::complete.cases(df),]
    
    compl <- list()
    for (i in 1:length(categoricalCols)) {
      compl[[i]] <- completeDf[[categoricalCols[i]]]
    }
    
    completeDf$newcol <- interaction(compl)
    levels <- unique(completeDf$newcol)
    means <- c()
    std <- c()
    minVal <- c()
    firstQuartile <- c()
    m <- c()
    thirdQuartile <- c()
    maxVal <- c()
    volumnRaw <- c()
    impact <- c()
    for (i in 1:length(levels)) {
      means[i] <- mean(completeDf[completeDf$newcol == levels[i],][[measureColumn]], na.rm = T)
      std[i] <- stats::sd(completeDf[completeDf$newcol == levels[i],][[measureColumn]], na.rm = T)
      minVal[i] <- min(completeDf[completeDf$newcol == levels[i],][[measureColumn]], na.rm = T)
      firstQuartile[i] <- stats::quantile(completeDf[completeDf$newcol == levels[i],][[measureColumn]], 0.25, na.rm = T)
      m[i] <- stats::median(completeDf[completeDf$newcol == levels[i],][[measureColumn]], na.rm = T)
      thirdQuartile[i] <- stats::quantile(completeDf[completeDf$newcol == levels[i],][[measureColumn]], 0.75, na.rm = T)
      maxVal[i] <- max(completeDf[completeDf$newcol == levels[i],][[measureColumn]], na.rm = T)
      volumnRaw[i] <- length(completeDf[completeDf$newcol == levels[i],][[measureColumn]])
    }
    cov <- std/means
    impact <- cov*volumnRaw
    measures <- rep(measureColumn, length(levels))
    # Output the table
    resTable <- data.frame(measure = measures, group = levels, Mean = means, 
                           Std = std, COV = cov, Min = minVal, 
                           Q1 = firstQuartile, Median = m, Q3 = thirdQuartile, Max = maxVal,
                           VolumnRaw = volumnRaw, Impact = impact)
    
    resTable <- roundNumericCols(resTable, digits = 3)
    outDf <- list(pvalueDF, resTable)
    names(outDf) <- c("Multiple-comparison-adjusted p-values for each pair of groups", 
                      "Basic statistics of each group")
  }
  
  return(outDf)

}
