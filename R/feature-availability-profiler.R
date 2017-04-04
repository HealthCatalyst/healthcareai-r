library(lubridate)

featureAvailabilityProfiler = function(
  df,
  admitColumnName='AdmitDTS',
  lastLoadColumnName='LastLoadDTS',
  showPlot=TRUE,
  debug=TRUE){

  # Error handling
  profilerErrorHandling(df, admitColumnName, lastLoadColumnName)

  # Create a few derived columns based on the hours since admit
  df$hoursSinceAdmit = hoursSinceAdmit(df[[admitColumnName]], df[[lastLoadColumnName]])
  
  # Calculate dates and times
  lastLoad = max(df[,lastLoadColumnName], na.rm=TRUE)
  oldestAdmitHours = max(df$hoursSinceAdmit, na.rm=TRUE)

  # Get the list of feature columns excluding the two date columns and the derived hours columns
  excludedColumnNames = c(lastLoadColumnName, admitColumnName, 'hoursSinceAdmit')
  featureColumns = findfeatureColumns(df, excludedColumnNames)

  if (debug){
    print('Here is a sample of your dataframe:')
    print(head(df))
    cat('Loaded ', dim(df)[1], ' rows and ', dim(df)[2], ' columns\n')
    cat('Data was last loaded on: ', lastLoad, ' (from', lastLoadColumnName, ')\n')
    cat('Oldest data is from ', oldestAdmitHours, ' (from', admitColumnName, ')\n')
    print('Columns that will be assessed for nulls')
    print(featureColumns)
  }

  # Initialize the result list
  result = list()

  # For each time bin
  hourBins = calculateHourBins(oldestAdmitHours)

  for(i in seq(1, length(hourBins))){
    # filter the resultset so you don't need to use the start/end feature of percentNullsInDateRange
    startInclusive = hourBins[i]
    if (is.na(hourBins[i + 1])){
      # If we are at the end of the hour bins, make a max beyond the largest bin
      endExclusive = max(hourBins) + 1
    } else {
      endExclusive = hourBins[i+1]
    }

    if(debug){
      cat('Calculating nulls for features from: ', startInclusive, ' to:', endExclusive, '\n')
    }

    tempSubset = dplyr::filter(df, df$hoursSinceAdmit >= startInclusive & df$hoursSinceAdmit < endExclusive)

    result$hoursSinceAdmit = append(result$hoursSinceAdmit, startInclusive)
    
    for(columnName in featureColumns){
      # Calculate the null percentage for the column
      tempNullPercentage = percentNullsInDateRange(tempSubset, admitColumnName='hoursSinceAdmit', featureColumnName=columnName)
      
      if (is.null(result[[columnName]])){
        # on the first run through the loop, initialize a new vector with the column name as the key
        result[[columnName]] = c(tempNullPercentage)
      } else {
        # on subsequen loops, append the value
        result[[columnName]] = append(result[[columnName]], tempNullPercentage)
      }
    }
  }
  
  if (showPlot){
    showPlot(result, featureColumns)
  }
  
  return(result)
}

hoursSinceAdmit = function(admitTimestamp, currentTime){
  # Given two dates in either YMD HMS string format or POSIXct, returns the difference in hours as a decimal number
  if (class(admitTimestamp) != 'POSIXct'){
    admitTimestamp = ymd_hms(admitTimestamp)
  }
  if (class(currentTime) != 'POSIXct'){
    currentTime = ymd_hms(currentTime)
  }
  
  delta = difftime(currentTime, admitTimestamp, units='hours')

  return(as.numeric(delta))
}

percentDataAvailableInDateRange = function(df, admitColumnName, featureColumnName, startInclusive=NULL, endExclusive=NULL, debug=FALSE){
  # Counts non-nulls (features actually populated) in a given feature column within a date range of a given date column.
  nullPercentage = percentNullsInDateRange(df, admitColumnName, featureColumnName, startInclusive=NULL, endExclusive=NULL, debug=FALSE)
  return(100 - nullPercentage)
}

percentNullsInDateRange = function(df, admitColumnName, featureColumnName, startInclusive=NULL, endExclusive=NULL, debug=FALSE){
  # Counts nulls in a given feature column within a date range of a given date column.
  # Inclusive on start and Excusive on end
  
  # Error handling
  if (missing(df)){
    stop('Please specify a dataframe')
  }
  if (missing(admitColumnName)){
    stop('Please specify an admit column name')
  }
  if (missing(featureColumnName)){
    stop('Please specify a feature column name')
  }
  
  if (!is.null(startInclusive) && !is.null(endExclusive)){
    # start and end dates exist, filter the dataframe
    subset = dplyr::filter(df, df[[admitColumnName]] >= startInclusive & df[[admitColumnName]] < endExclusive)
    
    if (debug){
      print('subset after filtering dates')
      print(subset)
      cat('totalRows:', totalRows, '\n')
    }
  } else {
    if (debug){
      print('No date range specified. Counting nulls in entire range.')
    }
    subset = df
  }

  # Count the nulls in the filtered dataframe
  missingThings = sum(is.na(subset[,featureColumnName]))
  totalRows = dim(subset)[1]
  
  percentNull = 100 * (missingThings/totalRows)
  
  if (debug){
    cat(featureColumnName, 'has', missing, 'nulls. (', percentNull, '%)\n')
  }

  return(percentNull)
}

calculateHourBins = function(oldestAdmitHours){
  # Given a number of hours, returns a vector of time bins in hours that the first day is divided into a few bins
  # and subsequent days are divided into 24 hour bins up to 90 days worth
  ninetyDaysWorthOfHours = 90 * 24

  if (oldestAdmitHours > ninetyDaysWorthOfHours){
    endHours = ninetyDaysWorthOfHours
  } else {
    endHours = oldestAdmitHours
  }
  
  # For the first day we are interested in hours 1, 2, 3, 4, 6, 8, 12
  firstDayHourBins = c(0, 1, 2, 3, 4, 6, 8, 12)

  # If there aren't any admits older than 24 hours, stop at 24 hours
  if (endHours <= 24){
    timeBins = append(firstDayHourBins, 24)
  } else {
    # After the first full day we only want daily bins (every 24 hours)
    beyondFirstDayHourBins = seq(24, endHours + 24, by=24)
    timeBins = append(firstDayHourBins, beyondFirstDayHourBins)
  }

  return(timeBins)
}

showPlot = function(result, keyList){
  # plot nulls for a list of columns over time.
  x = result$hoursSinceAdmit
  # Plot the first feature column
  y = result[[keyList[1]]]

  tempColor = randomColorGenerator()
  colors = c(tempColor)

  plot(
    x,
    y,
    xlab='Hours Since Admit',
    ylab='Percent Nulls',
    ylim=c(0, 100),
    xlim=c(min(result$hoursSinceAdmit), max(result$hoursSinceAdmit)),
    main='Feature Availability Over Time',
    type='l',
    col=tempColor
  )
  cat('plotting key ', keyList[1], 'with color: ', tempColor, '\n')

  # plot the remaining feature columns (skipping the first one)
  for (key in keyList[-1]){
    tempColor = randomColorGenerator()
    colors = append(colors, tempColor)
    lines(x=x, y=result[[key]], col=tempColor)
    # points(x=x, y=result[[key]], col=tempColor)
    cat('plotting key ', key, 'with color: ', tempColor, '\n')
  }

  legend(
    20,
    100,
    keyList,
    cex=0.5,
    bty='n',
    col=colors,
    lty=1:1,
  )
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
  if (class(df[[admitColumnName]]) != 'POSIXct'){
    tryCatch({
      df[[admitColumnName]] = ymd_hms(df[[admitColumnName]])
      }, warning = function(w){
        stop('Admit Date column is not a date type, or could not be parsed into one')
      }
    )
  }
  if (class(df[[lastLoadColumnName]]) != 'POSIXct'){
    tryCatch({
      df[[lastLoadColumnName]] = ymd_hms(df[[lastLoadColumnName]])
      }, warning = function(w){
        stop('Last Load Date column is not a date type, or could not be parsed into one')
      }      
    )
  }
}

findfeatureColumns = function(df, exclusions){
  allColumnNames = names(df)
  targetColumns = allColumnNames[-which(allColumnNames %in% exclusions)]

  return(targetColumns)
}