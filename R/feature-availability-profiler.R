require(lubridate)

featureAvailabilityProfiler = function(
  df,
  admitColumnName='AdmitDTS',
  lastLoadColumnName='LastLoadDTS',
  showPlot=TRUE,
  debug=TRUE){

  # Error handling
  profilerErrorHandling(df, admitColumnName, lastLoadColumnName)


  # Create a few derived columns based on the hours since admit
  df$hoursSinceAdmit = hoursSinceAdmit(df[[lastLoadColumnName]], df[[admitColumnName]])
  df$hoursSinceAdmitRounded = round(df$hoursSinceAdmit)
  
  # Calculate dates and times
  lastLoad = max(df[,lastLoadColumnName])
  oldestAdmitHours = max(df[[hoursSinceAdmitRounded]])
  
  # Get the list of feature columns
  excludedColumnNames = c(lastLoadColumnName, admitColumnName, 'hoursSinceAdmit', 'hoursSinceAdmitRounded')
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

  # Initialize the result list with each target column
  result = list(hoursSinceAdmitRounded=c())
  for(columnName in featureColumns){
      result[[columnName]] = list()
  }

  # For each time bin
  for(i in calculateHourBins(oldestAdmitHours)){
    # TODO
    # [x] 1. subtract "now" from admit date
    # [x] 2. change this to hours and
    # [x] 3. round it (to fake binning)
    # [x] 4. for each bin
      # [x] 5. for each column
    # [x] relate this integer to i relative
    # [ ] rework the start/end range in since there can be fractional

    # filter the resultset so you don't need to use the start/end feature of percentNullsInDateRange
    tempSubset = dplyr::filter(df, df$hoursSinceAdmitRounded == i)

    result$hoursSinceAdmitRounded = append(result$hoursSinceAdmitRounded, i)
    
    # Calculate the null percentage in each column
    for(columnName in featureColumns){
      tempNullPercentage = percentNullsInDateRange(tempSubset, admitColumnName='hoursSinceAdmitRounded', featureColumnName=columnName)
      result[[columnName]] = append(result[[columnName]], tempNullPercentage)
    }
  }
  
  # TODO probably not needed
  # result[['hoursSinceAdmitRounded']] = lapply(result[['hoursSinceAdmitRounded']], round, 1)
  
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
  firstDay = c(1/24, 2/24, 3/24, 4/24, 6/24, 8/24, 12/24)

  # After the first full day we only want daily bins (every 24 hours)
  wholeDays = seq(24, endHours, 24)
  
  timeBins = append(firstDay, wholeDays)
  return(timeBins)
}

showPlot = function(result, keyList){
  # plot nulls for a list of columns over time.

  # TODO Needs to be ported to R
  x = result[1]
  y = result[2]
  print('x: ')
  print(x)
  print('y: ')
  print(y)
  plot(x, y, main='Feature Availability Over Time')
  # plt.plot(result)
  # plt.plot(lw=2, linestyle='--')
  # plt.xlabel('Days since Admission')
  # plt.ylabel('Populated Values (%)')
  # plt.title()
  # plt.legend(labels=keyList, loc="lower right")
  # plt.show()
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
  # exclude the two date columns and the derived hours columns
  targetColumns = allColumnNames[-which(allColumnNames %in% exclusions)]

  return(targetColumns)
}