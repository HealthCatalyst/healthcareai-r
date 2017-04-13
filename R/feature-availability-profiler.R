#' @title
#' Display feature profile over time
#'
#' @description
#' Shows what percentage of data is avilable after a particular starting
#' time period.
#'
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
  
  tempColor = randomColorGenerator()
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
  for (key in keyList[-1]){
    tempColor = randomColorGenerator()
    colors = append(colors, tempColor)
    lines(x=x, y=result[[key]], col=tempColor)
    cat('plotting key ', key, 'with color: ', tempColor, '\n')
  }
  
  legend(
    legend = keyList,
    x = "bottomright",
    cex = 0.5,
    bty = 'n',
    col = colors,
    lty = 1:1,
  )
}

#' @export

percentDataAvailableInDateRange = function(df, 
                                           admitColumnName,
                                           startInclusive=NULL, 
                                           endExclusive=NULL) {
  
  # Counts nulls in a given feature col within a date range of a given date col
  # Inclusive on start and Excusive on end
  
  # Handle empty dataframes!!!!!!!!!!
  
  # Error handling
  if (missing(df)) {
    stop('Please specify a dataframe')
  }
  if (missing(admitColumnName)) {
    stop('Please specify an admit column name')
  }
  
  if (!is.null(startInclusive) && !is.null(endExclusive)) {
    # start and end dates exist, filter the dataframe
    subset = dplyr::filter(df, 
                           df[[admitColumnName]] >= startInclusive & df[[admitColumnName]] < endExclusive)
    
  } else {
    
    subset = df
  }
  
  percentFilled <- countPercentNotEmpty(subset) * 100
  
  return(percentFilled)
}

#' @export

calculateHourBins = function(oldestAdmitHours){
  # Given a number of hours, returns a vector of bins in hours;
  # that the first day is divided into multiple days are divided into 24 h bins 
  # up to 90 days worth
  ninetyDaysWorthOfHours = 90 * 24
  
  if (oldestAdmitHours > ninetyDaysWorthOfHours){
    endHours = ninetyDaysWorthOfHours
  } else {
    endHours = oldestAdmitHours
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

#### df is not df going into this function!
# Move function to healthcare.ai
countPercentNotEmpty <- function(df) {
  colList <- colMeans(!is.na(df))
  colList
}

randomColorGenerator = function(){
  # Return a random string representing an rgb value
  r = runif(1, 0, 1)
  g = runif(1, 0, 1)
  b = runif(1, 0, 1)
  return(rgb(r, g, b))
}

profilerErrorHandling = function(df, admitColumnName, lastLoadColumnName){
  # Make sure that it's a dataframe
  if (missing(df) || class(df) != 'data.frame' ){
    stop('Please specify a dataframe')
  }
  # With at least 3 columns
  if (dim(df)[2] < 3){
    stop('Dataframe must be at least 3 columns')
  }
  
  # If not already dates, try to parse them as such
  if (grepl(class(df[[admitColumnName]]),'POSIXct')) {
    print('original df length')
    print(length(df[[admitColumnName]]))
    df = individualDateParser(df, admitColumnName)
    # tryCatch({
    #   df[[admitColumnName]] = ymd_hms(df[[admitColumnName]], truncate=3)
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
  admitColumnName='AdmitDTS',
  lastLoadColumnName='LastLoadDTS',
  plotProfiler=TRUE,
  debug=TRUE){
  
  # Error handling
  profilerErrorHandling(df, admitColumnName, lastLoadColumnName)
  
  df[[admitColumnName]] <- as.POSIXct(df[[admitColumnName]])
  df[[lastLoadColumnName]] <- as.POSIXct(df[[lastLoadColumnName]])
  
  # Create a few derived columns based on the hours since admit
  df$hoursSinceAdmit = as.numeric(difftime(df[[lastLoadColumnName]], 
                                           df[[admitColumnName]], 
                                           units = 'hours'))
  
  # Calculate dates and times
  oldestAdmitHours = max(df$hoursSinceAdmit, na.rm = TRUE)
  lastLoad = max(df[[lastLoadColumnName]], na.rm = TRUE)
  
  # Get the list of feature cols (excluding two date cols and date diff)
  excludedColumnNames = c(lastLoadColumnName, 
                          admitColumnName, 
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
          oldestAdmitHours, 
          ' (from', admitColumnName, ')\n')
  print('Columns that will be assessed for nulls')
  print(featureColumns)
  
  # Initialize the result list
  result = list()
  
  # For each time bin
  hourBins = calculateHourBins(oldestAdmitHours)
  
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
    
    result$hoursSinceAdmit = append(result$hoursSinceAdmit, startInclusive)
    
    
    for (columnName in featureColumns) {
      
      # Calculate the null percentage for the column
      tempPercentAvailable = percentDataAvailableInDateRange(tempSubset, 
                                                             admitColumnName = 'hoursSinceAdmit')
      
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