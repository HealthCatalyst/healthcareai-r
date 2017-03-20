featureAvailabilityProfiler = function(
  df,
  admitColumnName='AdmitDTS',
  lastLoadColumnName='LastLoadDTS',
  showPlot=TRUE,
  showList=FALSE){
  
  # Error handling
  # if (df[[admitColumnName]].dtype != 'datetime64[ns]'){
  #   stop('Admit Date column is not a date type')
  # }
  # if (df[[lastLoadColumnName]].dtype != 'datetime64[ns]'){
  #   stop('Last Load Date column is not a date type')
  # }
  if (dim(df)[2] < 3){
    stop('Dataframe must be at least 3 columns')
  }
  
  # Look at data that's been pulled in
  cat(head(df))
  cat('Loaded ', dim(df)[1], ' rows and ', dim(df)[2], ' columns')
  
  # Get most recent date
  lastLoad = max(df[,lastLoadColumnName])
  
  cat('Data was last loaded on ', str(lastLoad), ' (from', admitColumnName, ')')
  oldestAdmit = min(df[[admitColumnName]])
  cat('Oldest data is from ', str(oldestAdmit), ' (from', admitColumnName, ')')
  
  # get key list to count
  result = list(Age=list())
  
  cat('Column names to count:')
  keyList = list()
  for(column in names(df)){
    if(!column %in% c(lastLoadColumnName, admitColumnName)){
      append(keyList, column)
      # create a container for final null counts
      result[[column]] = list()
    }
  }
  
  # For each time period
  for(i in calculateDateRange(lastLoad, oldestAdmit)){
    start = lastLoad - timedelta(days=i)
    
    append(result[['Age']], i)
    
    # Count the nulls in each column
    for(key in keyList){
      append(result[[key]], percentNullsInDateRange(df, start, lastLoad, admitColumnName))}
  }
  
  result[['Age']] = lapply(result[['Age']], round, 1)
  
  # result.set_index('Age', inplace=True)
  cat('Age is the number of days since patient admission.')
  
  if (showList){
    cat(result)
  }
  
  if (showPlot){
    showPlot(plt, result, keyList)
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
    missing = sum(is.na(subset[,featureColumnName]))
    totalRows = dim(subset)[1]
    
    if (debug){
      print('subset after filtering dates')
      print(subset)
      cat('totalRows:', totalRows, '\n')
    }
  } else {
    missing = sum(is.na(df[,featureColumnName]))
    totalRows = dim(df)[1]
  }
  
  percentNull = 100 * (missing/totalRows)
  if (debug){
    cat(featureColumnName, 'has', missing, 'nulls. (', percentNull, '%)\n')
  }
  return(percentNull)
}

calculateDateRange = function(lastLoad, oldestAdmit){
  # get date range to count over
  dateSpread = lastLoad - oldestAdmit
  
  if (dateSpread.days < 90){
    endDate = dateSpread.days
  } else {
    endDate = 91
  }
  
  dateRange = append(list(1/24, 2/24, 4/24, 8/24 ,12/24), list(range(1, endDate)))
  return(dateRange)
}

showPlot = function(plt, result, keyList){
  # plot nulls for a list of columns over time.
  plt.plot(result)
  plt.plot(lw=2, linestyle='--')
  plt.xlabel('Days since Admission')
  plt.ylabel('Populated Values (%)')
  plt.title('Feature Availability Over Time')
  plt.legend(labels=keyList, loc="lower right")
  plt.show()
}
