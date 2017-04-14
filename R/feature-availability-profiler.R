#' @title
#' Display availability feature profile over time
#'
#' @description
#' Shows what percentage of data is avilable after a particular starting
#' time period.
#' @param result A 2-d list, where features go down the rows and percent
#' filled by hour are the columns 
#' @param keyList A vector of strings, representing the features.
#' @return Nothing
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}

plotProfiler = function(result, keyList){
  # TODO: Just pull the keyList for the name of each row in result 2-d List
  
  # plot nulls for a list of columns over time.
  x = result$hoursSinceAdmit
  # Plot the first feature column
  y = result[[keyList[1]]]
  
  tempColor = rgb(runif(1, 0, 1),runif(1, 0, 1),runif(1, 0, 1))
  colors = c(tempColor)
  
  plot(
    x,
    y,
    xlab = 'Hours Since Admit',
    ylab = 'Percent Feature Availability',
    ylim = c(0, 100),
    xlim = c(min(result$hoursSinceAdmit), max(result$hoursSinceAdmit)),
    main = 'Feature Availability Over Time',
    type = 'l',
    col = tempColor
  )
  
  # plot the remaining feature columns (skipping the first one)
  for (key in keyList[-1]) {
    tempColor = rgb(runif(1, 0, 1),runif(1, 0, 1),runif(1, 0, 1))
    colors = append(colors, tempColor)
    lines(x=x, y=result[[key]], col = tempColor)
  }
  
  legend(
    legend = keyList,
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
#' @description
#' Given a number of hours, generate a reasonable vector of bins in hours such 
#' that the first day is divided into multiple days are divided into 24 h bins 
#' up to 90 days worth
#'
#' @param lastHourOfInterest Number representing the last hour of interest
#' @return numeric vector of hours, reasonably spaced
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}

calculateHourBins = function(lastHourOfInterest){
  # Given a number of hours, 
  ninetyDaysWorthOfHours = 90 * 24
  
  if (lastHourOfInterest > ninetyDaysWorthOfHours) {
    endHours = ninetyDaysWorthOfHours
  } else {
    endHours = lastHourOfInterest
  }
  
  # For the first day we are interested in hours 1, 2, 3, 4, 6, 8, 12
  firstDayHourBins = c(0, 1, 2, 3, 4, 6, 8, 12)
  
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

#' @export

featureAvailabilityProfiler = function(
  df,
  startDateColumn='AdmitDTS',
  lastLoadDateColumnName='LastLoadDTS',
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
  
  # TODO: Make sure specified date cols are in data frame
  
  # Convert specified date columns to dates, if necessary
  if (class(df[[startDateColumn]])[1] != 'POSIXct') {
    tryCatch(
      df[[startDateColumn]] <- as.POSIXct(df[[startDateColumn]]),
      error = function(e) {
      e$message <- paste0(startDateColumn, " may not be a date column. \n", e)
      stop(e)
    })
  }

  if (class(df[[lastLoadDateColumnName]])[1] != 'POSIXct') {
    tryCatch(
      df[[lastLoadDateColumnName]] <- as.POSIXct(df[[lastLoadDateColumnName]]),
      error = function(e) {
      e$message <- paste0(lastLoadDateColumnName,
                          " may not be a date column. \n", e)
      stop(e)
    })
  }
  
  # Create a few derived columns based on the hours since admit
  df$hoursSinceAdmit = as.numeric(difftime(df[[lastLoadDateColumnName]], 
                                           df[[startDateColumn]], 
                                           units = 'hours'))
  
  # Calculate dates and times
  firstAdmit = max(df[[startDateColumn]], na.rm = TRUE)
  lastLoad = max(df[[lastLoadDateColumnName]], na.rm = TRUE)
  largestAdmitLoadDiff = max(df$hoursSinceAdmit, na.rm = TRUE)
  
  # Get the list of feature cols (excluding two date cols and date diff)
  excludedColumnNames = c(lastLoadDateColumnName, 
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
          ' (from ', lastLoadDateColumnName, ')\n')
  
  message('Columns that will be assessed for nulls:')
  for (i in 1:length(featureColumns)) {
    message(featureColumns[i])
  }
  
  # Initialize the result list
  result = list()
  
  # For each time bin
  hourBins = calculateHourBins(largestAdmitLoadDiff)
  
  for (i in 1:length(hourBins)) {
    # filter the resultset so you don't need to use the start/end feature of percentNullsInDateRange
    startInclusive = hourBins[i]
    if (is.na(hourBins[i + 1])) {
      # If we are at the end of the hour bins, make a max beyond the largest bin
      endExclusive = max(hourBins) + 1
    } else {
      endExclusive = hourBins[i + 1]
    }
    
    message('\nCalculating nulls for features from hrs: ', 
            startInclusive, ' to:',
            endExclusive, '\n')
    
    tempSubset <- df[ which(df$hoursSinceAdmit >= startInclusive &
                            df$hoursSinceAdmit < endExclusive), ]
    
    result$hoursSinceAdmit = append(result$hoursSinceAdmit, startInclusive)
    
    for (columnName in featureColumns) {
      
      # Calculate the null percentage for the column
      tempPercentAvailable = percentDataAvailableInDateRange(df = tempSubset)
      
      if (is.null(result[[columnName]])){
        # on the first run through the loop, initialize a new vector with the column name as the key
        result[[columnName]] = c(tempPercentAvailable[[columnName]])
      } else {
        # on subsequent loops, append the value
        result[[columnName]] = append(result[[columnName]], tempPercentAvailable[[columnName]])
      }
    }
  }
  
  if (plotProfiler) {
    plotProfiler(result, featureColumns)
  }
  
  return(result)
}