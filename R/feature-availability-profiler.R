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
  cat('plotting key ', keyList[1], 'with color: ', tempColor, '\n')
  
  # plot the remaining feature columns (skipping the first one)
  for (key in keyList[-1]) {
    tempColor = rgb(runif(1, 0, 1),runif(1, 0, 1),runif(1, 0, 1))
    colors = append(colors, tempColor)
    lines(x=x, y=result[[key]], col = tempColor)
    cat('plotting key ', key, 'with color: ', tempColor, '\n')
  }
  
  legend(
    legend = keyList,
    x = "bottomright",
    cex = 0.5,
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

# randomColorGenerator = function(){
#   # Return a random string representing an rgb value
#   r = runif(1, 0, 1)
#   g = runif(1, 0, 1)
#   b = runif(1, 0, 1)
#   return(rgb(r, g, b))
# }

profilerErrorHandling = function(df, dateColumn, lastLoadColumnName){
  # Make sure that it's a dataframe
  if (missing(df) || class(df) != 'data.frame' ){
    stop('Please specify a dataframe')
  }
  # With at least 3 columns
  if (dim(df)[2] < 3){
    stop('Dataframe must be at least 3 columns')
  }
  
  # If not already dates, try to parse them as such
  if (grepl(class(df[[dateColumn]]),'POSIXct')) {
    print('original df length')
    print(length(df[[dateColumn]]))
    df = individualDateParser(df, dateColumn)
    # tryCatch({
    #   df[[dateColumn]] = ymd_hms(df[[dateColumn]], truncate=3)
    #   }, warning = function(w){
    #     print('Admit Date column is not a date type, or could not be parsed into one')
    #   }
    # )
  }
  if (class(df[[lastLoadColumnName]]) != 'POSIXct'){
    #since the column approach dies if there is one failure, take a loopin approach
    print('original df length')
    print(length(df[[lastLoadColumnName]]))
    df = individualDateParser(df, lastLoadColumnName)
    
    # tryCatch({
    #   df[[lastLoadColumnName]] = ymd_hms(df[[lastLoadColumnName]], truncate=3)
    #   }, warning = function(w){
    #     print('Last Load Date column is not a date type, or could not be parsed into one')
    #   }      
    # )
  }
}

#' @export

individualDateParser = function(df, dateColumn){
  dates <- c()
  
  print('dataframe length')
  print(length(df[[dateColumn]]))
  
  for (date in df[[dateColumn]]){
    # tryCatch({
    # dates = append(dates, ymd_hms(date))
    converted <- ymd_hms(date)
    dates <- c(dates, converted)
    print(converted)
    
    print(length(dates))
    print(typeof(dates))
    cat(dates, '\n')
    #   }, warning = function(w){
    #     print('failed to parse this date:')
    #     print(date)
    #     # dates = append(dates, NA)
    #     # TODO deal with NA/s down the road
    #     # consider raising an error to have the analyst clean the data
    #   }
    # )
  }
  
  df$newCOlumn <- dates
  
  return(df)
}

#' @export

featureAvailabilityProfiler = function(
  df,
  startDateColumn='AdmitDTS',
  lastLoadColumnName='LastLoadDTS',
  plotProfiler=TRUE,
  debug=TRUE){
  
  # Error handling
  profilerErrorHandling(df, startDateColumn, lastLoadColumnName)
  
  df[[startDateColumn]] <- as.POSIXct(df[[startDateColumn]])
  df[[lastLoadColumnName]] <- as.POSIXct(df[[lastLoadColumnName]])
  
  # Create a few derived columns based on the hours since admit
  df$hoursSinceAdmit = as.numeric(difftime(df[[lastLoadColumnName]], 
                                           df[[startDateColumn]], 
                                           units = 'hours'))
  
  # Calculate dates and times
  lastHourOfInterest = max(df$hoursSinceAdmit, na.rm = TRUE)
  lastLoad = max(df[[lastLoadColumnName]], na.rm = TRUE)
  
  # Get the list of feature cols (excluding two date cols and date diff)
  excludedColumnNames = c(lastLoadColumnName, 
                          startDateColumn, 
                          'hoursSinceAdmit')
  allColumnNames = names(df)
  featureColumns = allColumnNames[-which(allColumnNames 
                                         %in% 
                                           excludedColumnNames)]
  
  print('Here is a sample of your dataframe:')
  print(head(df))
  message('Loaded ', dim(df)[1], ' rows and ', dim(df)[2], ' columns\n')
  message('Data was last loaded on: ', 
          lastLoad, 
          ' (from', lastLoadColumnName, ')\n')
  message('Oldest data is from ', 
          lastHourOfInterest, 
          ' (from', startDateColumn, ')\n')
  print('Columns that will be assessed for nulls')
  print(featureColumns)
  
  # Initialize the result list
  result = list()
  
  # For each time bin
  hourBins = calculateHourBins(lastHourOfInterest)
  
  for (i in 1:length(hourBins)) {
    # filter the resultset so you don't need to use the start/end feature of percentNullsInDateRange
    startInclusive = hourBins[i]
    if (is.na(hourBins[i + 1])) {
      # If we are at the end of the hour bins, make a max beyond the largest bin
      endExclusive = max(hourBins) + 1
    } else {
      endExclusive = hourBins[i + 1]
    }
    
    if (debug) {
      cat('Calculating nulls for features from: ', startInclusive, ' to:', endExclusive, '\n')
    }
    
    print('i')
    print(i)
    
    print('hourBin')
    print(hourBins[i])
    
    print('checking df before subsetting')
    print(df)
    print(summary(df))
    
    tempSubset = dplyr::filter(df, df$hoursSinceAdmit >= startInclusive & df$hoursSinceAdmit < endExclusive)
    
    #reduced <- df[ which(as.Date(df[[dateColumn]]) >= as.Date(startInclusive) &
    #                       as.Date(df[[dateColumn]]) < as.Date(endExclusive)),]
    
    result$hoursSinceAdmit = append(result$hoursSinceAdmit, startInclusive)
    
    # df, 
    # dateColumn=NULL,
    # startInclusive=NULL, 
    # endExclusive=NULL
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
  
  print('Result after filling')
  print(result)
  
  if (plotProfiler) {
    plotProfiler(result, featureColumns)
  }
  
  return(result)
}