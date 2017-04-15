#' @title
#' Display availability feature profile over time
#'
#' @description Shows what percentage of data is avilable after a particular 
#' starting time period.
#' @param listOfVectors A list of vectors, where first vector has the hours and
#' subsequent vectors represent the features and how much they're filled
#' for each of the hours
#' @return Nothing
#'
#' @export
#' @references \url{http://healthcare.ai}
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

#' @title
#' Calculate a vector of reasonable time bins
#' 
#' @description Given a number of hours, generate a reasonable vector of bins in hours such 
#' that the first day is divided into multiple days are divided into 24 h bins 
#' up to 90 days worth
#'
#' @param lastHourOfInterest Number representing the last hour of interest
#' @return numeric vector of hours, reasonably spaced
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' result <- calculateHourBins(90)
#' result

calculateHourBins = function(lastHourOfInterest){
  # Given a number of hours, 
  ninetyDaysWorthOfHours = 90 * 24
  
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
#' @references \url{http://healthcare.ai}
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
  if (!(startDateColumn %in% colnames(df))) {
    stop(paste0(startDateColumn,' is not in your dataframe.',
         ' Please carefully specify the startDateColumn'))
  }
  
  if (!(lastLoadDateColumn %in% colnames(df))) {
    stop(paste0(lastLoadDateColumn,' is not in your dataframe.',
         ' Please carefully specify the lastLoadDateColumn'))
  }
  
  # Convert specified date columns to dates, if necessary
  if (class(df[[startDateColumn]])[1] != 'POSIXct') {
    tryCatch(
      df[[startDateColumn]] <- as.POSIXct(df[[startDateColumn]]),
      error = function(e) {
      e$message <- paste0(startDateColumn, " may not be a date column. \n", e)
      stop(e)
    })
  }

  if (class(df[[lastLoadDateColumn]])[1] != 'POSIXct') {
    tryCatch(
      df[[lastLoadDateColumn]] <- as.POSIXct(df[[lastLoadDateColumn]]),
      error = function(e) {
      e$message <- paste0(lastLoadDateColumn,
                          " may not be a date column. \n", e)
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
  
  message('Loaded ', dim(df)[1], ' rows and ', dim(df)[2], ' columns\n')
  message('Earliest admit (or equivalent) is from: ', 
          firstAdmit,
          ' (from ', startDateColumn, ')\n')
  message('Data was last loaded on: ', 
          lastLoad, 
          ' (from ', lastLoadDateColumn, ')\n')
  
  message('Columns that will be assessed for nulls:')
  for (i in 1:length(featureColumns)) {
    message(featureColumns[i])
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
    
    message('\nCalculating nulls for features from hrs: ', 
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