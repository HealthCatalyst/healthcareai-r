#' @title
#' Calculate a vector of reasonable time bins
#' 
#' @description Given a number of hours, generate a reasonable vector of bins 
#' in hours such that the first day is divided into multiple days are divided 
#' into 24 h bins up to 90 days worth. Typically used with 
#' featureAvailabilityProfiler
#'
#' @param lastHourOfInterest Number (hour) scalar representing the last hour of 
#' interest
#' @return numeric vector of hours, reasonably spaced
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' result <- calculateHourBins(90)
#' result
calculateHourBins = function(lastHourOfInterest){
  # Given a number of hours, 
  ninetyDaysWorthOfHours = 90 * 24
  
  if (!class(lastHourOfInterest) %in% c('numeric','integer')) {
    stop('You must input a number.')
  } 
  
  if (lastHourOfInterest > ninetyDaysWorthOfHours) {
    endHours = ninetyDaysWorthOfHours
  } else {
    endHours = lastHourOfInterest
  }
  
  # For the first day we are interested in hours 1, 2, 3, 4, 6, 8, 12, 18
  firstDayHourBins = c(0, 1, 2, 3, 4, 6, 8, 12, 18)
  
  # If there aren't any admits older than 24 hours, stop at 24 hours
  if (endHours <= 24) {
    timeBins = append(firstDayHourBins, 24)
  } else {
    # After the first full day we only want daily bins (every 24 hours)
    beyondFirstDayHourBins = seq(24, endHours + 24, by = 24)
    timeBins = append(firstDayHourBins, beyondFirstDayHourBins)
  }
  
  return(timeBins)
}

#' @title
#' DEPRECATED. Calculates percentage of each column in df that is NULL (NA)
#'
#' @description Returns a vector with percentage of each column that is NULL
#' in the original data frame
#' @param df A data frame
#' @return A vector that contains the percentage of NULL in each column
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' df = data.frame(a=c(1,2,NA,NA,3),
#'                 b=c(NA,NA,NA,NA,NA),
#'                 c=c(NA,NA,'F','M',NA))
#' colList = countPercentEmpty(df)
#' colList
countPercentEmpty <- function(df) {
  warning(paste0('This function has been deprecated and will be removed',
                 ' after v0.1.12.\n',
                 'Please instead see ?percentDataAvailableInDateRange.'))
  colList <- colMeans(is.na(df))
  colList
}

#' @title
#' Calculate and plot data availability over time
#' 
#' @description Helps you determine how much data is present in each feature, 
#' by hour, after a particular event (like patient admit)
#' 
#' @param df A dataframe
#' @param startDateColumn Optional string of the column name, representing the 
#' date of the starting event of interest (e.g., patient admit)
#' @param lastLoadDateColumn Optional string of the column name, representing
#' the date the row was loaded into the final dataset (i.e., via daily ETL)
#' @param plotProfiler Default is TRUE. Whether to plot profiler results 
#' @return a list, that has as many vectors as columns in the original 
#' dataframe, with each vector holding the percentage full for each hour
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples 
#' df <- data.frame(a = c(2,1,3,5,4,NA,7,NA),
#'                  b = c(0.7,-2,NA,-4,-5,-6,NA,NA),
#'                  c = c(100,300,200,NA,NA,NA,NA,500),
#'                  d = c(407,500,506,504,NA,NA,NA,405),
#'                  admit = c('2012-01-01 00:00:00','2012-01-01 00:00:00',
#'                            '2012-01-01 12:00:00','2012-01-01 12:00:00',
#'                            '2012-01-02 00:00:00','2012-01-02 00:00:00',
#'                            '2012-01-02 12:00:00','2012-01-02 12:00:00'),
#'                  loaded = c('2012-01-03 00:00:00','2012-01-03 00:00:00',
#'                             '2012-01-03 00:00:00','2012-01-03 00:00:00',
#'                             '2012-01-03 00:00:00','2012-01-03 00:00:00',
#'                             '2012-01-03 00:00:00','2012-01-03 00:00:00'))
#' str(df)
#' head(df)
#' 
#' d <- featureAvailabilityProfiler(df = df,
#'                                  startDateColumn = 'admit',
#'                                  lastLoadDateColumn = 'loaded')
#' d # Look at the data in the console                                
featureAvailabilityProfiler = function(
  df,
  startDateColumn='AdmitDTS',
  lastLoadDateColumn='LastLoadDTS',
  plotProfiler=TRUE) {
  
  # Error handling
  # Make sure that it's a dataframe
  if (missing(df) || class(df) != 'data.frame' ) {
    stop('Please specify a dataframe')
  }
  
  # With at least 3 columns
  if (dim(df)[2] < 3) {
    stop('Dataframe must be at least 3 columns')
  }
  
  # Check that date columns are in dataframe
  if (!(startDateColumn %in% names(df))) {
    stop(paste0(startDateColumn,' is not in your dataframe.',
                ' Please carefully specify the startDateColumn'))
  }
  
  if (!(lastLoadDateColumn %in% names(df))) {
    stop(paste0(lastLoadDateColumn,' is not in your dataframe.',
                ' Please carefully specify the lastLoadDateColumn'))
  }
  
  # Convert specified date columns to dates, if necessary
  if (class(df[[startDateColumn]])[1] != 'POSIXct') {
    tryCatch(
      df[[startDateColumn]] <- as.POSIXct(df[[startDateColumn]]),
      error = function(e) {
        e$message <- paste0(startDateColumn, " may not be a datetime column,",
                            " or the column may not be in format YYYY-MM-DD\n", e)
        stop(e)
      })
  }
  
  if (class(df[[lastLoadDateColumn]])[1] != 'POSIXct') {
    tryCatch(
      df[[lastLoadDateColumn]] <- as.POSIXct(df[[lastLoadDateColumn]]),
      error = function(e) {
        e$message <- paste0(lastLoadDateColumn,
                            " may not be a datetime column,",
                            " or the column may not be in format YYYY-MM-DD\n", e)
        stop(e)
      })
  }
  
  # Create a few derived columns based on the hours since admit
  df$hoursSinceAdmit = as.numeric(difftime(df[[lastLoadDateColumn]], 
                                           df[[startDateColumn]], 
                                           units = 'hours'))
  
  # Calculate dates and times
  firstAdmit = max(df[[startDateColumn]], na.rm = TRUE)
  lastLoad = max(df[[lastLoadDateColumn]], na.rm = TRUE)
  largestAdmitLoadDiff = max(df$hoursSinceAdmit, na.rm = TRUE)
  
  # Get the list of feature cols (excluding two date cols and date diff)
  excludedColumnNames = c(lastLoadDateColumn, 
                          startDateColumn, 
                          'hoursSinceAdmit')
  allColumnNames = names(df)
  featureColumns = allColumnNames[-which(allColumnNames 
                                         %in% 
                                           excludedColumnNames)]
  
  cat('Loaded ', dim(df)[1], ' rows and ', dim(df)[2], ' columns\n')
  cat('Earliest admit (or equivalent) is from: ', 
      firstAdmit,
      ' (from ', startDateColumn, ')\n')
  cat('Data was last loaded on: ', 
      lastLoad, 
      ' (from ', lastLoadDateColumn, ')\n')
  
  cat('Columns that will be assessed for nulls:')
  for (i in 1:length(featureColumns)) {
    cat(featureColumns[i])
  }
  
  # Initialize the result list
  result = list()
  
  # For each time bin
  hourBins = calculateHourBins(largestAdmitLoadDiff)
  
  for (i in 1:length(hourBins)) {
    # Set particular time slice to focus on
    startInclusive = hourBins[i]
    
    if (is.na(hourBins[i + 1])) {
      # If we are at the end of the hour bins, make a max beyond the largest bin
      endExclusive = max(hourBins) + 1
    } else {
      endExclusive = hourBins[i + 1]
    }
    
    cat('\nCalculating nulls for features from hrs: ', 
        startInclusive, ' to ',
        endExclusive, '\n')
    
    tempSubset <- df[ which(df$hoursSinceAdmit >= startInclusive &
                              df$hoursSinceAdmit < endExclusive), ]
    
    result$hoursSinceAdmit = append(result$hoursSinceAdmit, startInclusive)
    
    for (columnName in featureColumns) {
      
      # Calculate the null percentage for the column
      tempPercentAvailable = percentDataAvailableInDateRange(df = tempSubset)
      
      if (is.null(result[[columnName]])) {
        # On 1st run through loop, initialize new vector with column name as key
        result[[columnName]] = tempPercentAvailable[[columnName]]
      } else {
        # On subsequent loops, append the value
        result[[columnName]] = append(result[[columnName]], 
                                      tempPercentAvailable[[columnName]])
      }
    }
  }
  
  if (plotProfiler) {
    plotProfiler(result)
  }
  
  return(result)
}


#' @title
#' Find the percent of a column that's filled
#'
#' @description
#' Shows what percentage of data is avilable (potentially within a specified 
#' date range)
#'
#' @param df A dataframe
#' @param dateColumn Optional string representing a date column of interest
#' @param startInclusive Optional string in the in this date style: 'YYYY-MM-DD'
#' @param endExclusive Optional string in the in this date style: 'YYYY-MM-DD'
#' @return A labeled numeric vector, representing each column in input df
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples 
#' df <- data.frame(a = c(1,2,NA,NA),
#'                  b = c('m','f','m','f'),
#'                  c = c(0.7,NA,2.4,-4),
#'                  d = c(100,300,200,NA),
#'                  e = c(400,500,NA,504),
#'                  datecol = c('2012-01-01','2012-01-02',
#'                              '2012-01-03','2012-01-07'))
#' 
#' out <- percentDataAvailableInDateRange(df = df, # <- Only required argument
#'                                        dateColumn = 'datecol',
#'                                        startInclusive = '2012-01-01',
#'                                        endExclusive = '2012-01-08')
#' out
percentDataAvailableInDateRange = function(df,
                                           dateColumn=NULL,
                                           startInclusive=NULL,
                                           endExclusive=NULL) {
  
  # Error handling
  if (missing(df)) {
    stop('Please specify a dataframe')
  }
  
  # TODO: Simplify this error logic ground the three date cols
  if ((missing(dateColumn)) &
      ((!missing(startInclusive)) |
       (!missing(endExclusive)))) {
    stop('If any, specify dateColumn, startInclusive, AND endExclusive')
  }
  
  if ((missing(startInclusive)) &
      ((!missing(endExclusive)) |
       (!missing(dateColumn)))) {
    stop('If any, specify dateColumn, startInclusive, AND endExclusive')
  }
  
  if ((missing(endExclusive)) &
      ((!missing(startInclusive)) |
       (!missing(dateColumn)))) {
    stop('If any, specify dateColumn, startInclusive, AND endExclusive')
  }
  
  # If specified, check if date col exists in dataframe
  if ((!missing(dateColumn)) && (!dateColumn %in% names(df))) {
    stop(dateColumn, ' is not a column in your dataframe')
  }
  
  # If one gets past error checking, and specified a dateColumn, subset data
  if (!missing(dateColumn)) {
    tryCatch( # Test this try catch and create unit test!!
      reduced <- df[(as.Date(df[[dateColumn]]) >= as.Date(startInclusive) &
                       as.Date(df[[dateColumn]]) < as.Date(endExclusive)),],
      error = function(e) {
        e$message <- paste0("The dateColumn, startInclusive, and endExclusive ",
                            "columns need dates to be in YYYY-MM-DD format\n", e)
        stop(e)
      })
  } else {
    reduced = df
  }
  
  reduced <- reduced[, !(names(reduced) %in% dateColumn)]
  
  percentFilled <- colMeans(!is.na(reduced)) * 100
  
  return(percentFilled)
}

#' @title
#' Display availability feature profile over time
#'
#' @description Shows what percentage of data is avilable after a particular 
#' starting time period.
#' @param listOfVectors A list of vectors, where first vector has the hours and
#' subsequent vectors represent the features and how much they're filled
#' for each of the hours. Usually populated by featureAvailabilityProfiler()
#' @return Nothing
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
#' @examples 
#' lis <- list()
#' # Establish hour range/sequence
#' lis$hoursSinceAdmit <- c(0,1,3,6,12,24)
#' 
#' # Add features and their percent full for each hour
#' lis$BP <- c(40, 45, 65, 78, 80, 90)
#' lis$LDL <- c(10, 30, 40, 70, 100, 120)
#' 
#' plotProfiler(lis)
plotProfiler = function(listOfVectors){
  # Establish hour range/sequence
  x = listOfVectors$hoursSinceAdmit
  # Plot the first feature column (i.e., first count vector in listOfVectors)
  y = unlist(listOfVectors[2])
  
  tempColor = grDevices::rgb(stats::runif(1, 0, 1),
                             stats::runif(1, 0, 1),
                             stats::runif(1, 0, 1))
  colors = c(tempColor)
  
  plot(
    x,
    y,
    xlab = 'Hours Since Admit',
    ylab = 'Percent Feature Availability',
    ylim = c(0, 100),
    xlim = c(min(listOfVectors$hoursSinceAdmit), 
             max(listOfVectors$hoursSinceAdmit)),
    main = 'Feature Availability Over Time',
    type = 'l',
    lwd = 3.,
    col = tempColor
  )
  
  # plot the remaining feature columns (skipping the first one)
  for (i in 3:length(listOfVectors)) {
    tempColor = grDevices::rgb(stats::runif(1, 0, 1),
                               stats::runif(1, 0, 1),
                               stats::runif(1, 0, 1))
    colors = append(colors, tempColor)
    graphics::lines(x = x, y = unlist(listOfVectors[i]), col = tempColor)
  }
  
  # Get vector of vector labels for legnd; start at 2 to avoid hoursSinceAdmit
  featureVector <- names(listOfVectors)[2:length(listOfVectors)]
  
  legend(
    legend = featureVector,
    x = "bottomright",
    pt.cex = 1,
    cex = 1.7,
    bty = 'n',
    col = colors,
    lty = 1:1
  )
}