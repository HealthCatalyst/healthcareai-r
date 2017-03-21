featureAvailabilityProfiler = function(
  df,
  admitColumnName='AdmitDTS',
  lastLoadColumnName='LastLoadDTS',
  showPlot=TRUE,
  debug=TRUE){
  
  # Error handling
  if (missing(df)){
    stop('Please specify a dataframe')
  }
  if (dim(df)[2] < 3){
    stop('Dataframe must be at least 3 columns')
  }
  if (class(df[[admitColumnName]]) != 'Date'){
    stop('Admit Date column is not a date type')
  }
  if (class(df[[lastLoadColumnName]]) != 'Date'){
    stop('Last Load Date column is not a date type')
  }
  
  lastLoad = max(df[,lastLoadColumnName])
  oldestAdmit = min(df[[admitColumnName]])
  
  if (debug){
    print('Here is a sample of your dataframe:')
    print(head(df))
    cat('Loaded ', dim(df)[1], ' rows and ', dim(df)[2], ' columns\n')
    cat('Data was last loaded on: ', lastLoad, ' (from', lastLoadColumnName, ')\n')
    cat('Oldest data is from ', oldestAdmit, ' (from', admitColumnName, ')\n')
  }
  

  result = list(Age=list())
  
  # get key list to count
  targetColumns = vector()
  for(column in names(df)){
    # exclude the two date columns 
    if(!column %in% c(lastLoadColumnName, admitColumnName)){
      # 
      targetColumns = append(targetColumns, column)
      # create a container for final null counts
      result[[column]] = list()
    }
  }
  
  if (debug){
    print('Columns that will be assessed for nulls')
    print(targetColumns)
  }

  # For each time period
  counter = 0
  for(i in calculateDateRange(lastLoad, oldestAdmit)){
    # subtract the fraction of the date range
    start = lastLoad - ddays(i)
    
    result[['Age']] = append(result[['Age']], i)
    
    # Calculate the null percentage in each column
    for(key in targetColumns){
      tempNullPercentage = percentNullsInDateRange(df, admitColumnName=admitColumnName, featureColumnName=key, start=start, end=lastLoad)
      result[[key]] = append(result[[key]], tempNullPercentage)
    }
    counter = counter + 1
    print(counter)
  }
  
  result[['Age']] = lapply(result[['Age']], round, 1)
  
  if (showPlot){
    showPlot(result, targetColumns)
  }
  
  return(result)
}

percentNullsInDateRange = function(df, admitColumnName, featureColumnName, start=NULL, end=NULL, debug=FALSE){
  # Counts nulls in a given feature column within a date range of a given date column.
  
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
  
  if (!is.null(start) && !is.null(end)){
    # start and end dates exist, subset the dataframe
    # TODO find out better way to do this subsetting in one step
    subset = dplyr::filter(df, df[[admitColumnName]] >= end & df[[admitColumnName]] <= end)
    missingThings = sum(is.na(subset[,featureColumnName]))
    totalRows = dim(subset)[1]
    
    if (debug){
      print('subset after filtering dates')
      print(subset)
      cat('totalRows:', totalRows, '\n')
    }
  } else {
    if (debug){
      print('No date range specified. Counting nulls in entire range.')
    }
    missingThings = sum(is.na(df[,featureColumnName]))
    totalRows = dim(df)[1]
  }
  
  percentNull = 100 * (missingThings/totalRows)
  if (debug){
    cat(featureColumnName, 'has', missing, 'nulls. (', percentNull, '%)\n')
  }

  return(percentNull)
}

calculateDateRange = function(lastLoad, oldestAdmit){
  # get date range to count over
  dateSpread = as.double(difftime(lastLoad, oldestAdmit, units='days'))
  
  if (dateSpread < 90){
    endDate = dateSpread
  } else {
    endDate = 91
  }
  
  dateRange = append(list(1/24, 2/24, 4/24, 8/24 ,12/24), list(range(1, endDate)))
  return(dateRange)
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
