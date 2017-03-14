df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,2))

percentNullsInDateRange(df)

featureAvailabilityProfiler = function(
  df,
  admitColumnName='AdmitDTS',
  lastLoadColumnName='LastLoadDTS',
  showPlot=TRUE,
  showList=FALSE){
  
  # Error handling
  if (df[[admitColumnName]].dtype != 'datetime64[ns]'){
    stop('Admit Date column is not a date type')
  }
  if (df[[lastLoadColumnName]].dtype != 'datetime64[ns]'){
    stop('Last Load Date column is not a date type')
  }
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
  
  if (showList is True){
    cat(result)
  }
  
  if (showPlot is True){
    showPlot(plt, result, keyList)
  }
  
  return(result)
}

percentNullsInDateRange = function(df, admitColumnName, featureColumnName, start=NULL, end=NULL){
  # Counts nulls within a date range.
  
  if (!is.null(start) && !is.null(end)){
    # start and end dates exist, subset the dataframe
    # TODO find out better way to do this subsetting in one step
    subset = df[df[[admitColumnName]] <= end,]
    subset = df[df[[admitColumnName]] >= start,]
    missing = sum(is.na(subset[,featureColumnName]))
    totalRows = dim(subset)[1]
  } else {
    missing = sum(is.na(df[,featureColumnName]))
    totalRows = dim(df)[1]
  }
  
  percentNull = 100 * (missing/totalRows)
  cat(featureColumnName, 'has', missing, 'nulls. (', percentNull, '%)\n')
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
  # plot nulls vs time.
  plt.plot(result)
  plt.plot(lw=2, linestyle='--')
  plt.xlabel('Days since Admission')
  plt.ylabel('Populated Values (%)')
  plt.title('Feature Availability Over Time')
  plt.legend(labels=keyList, loc="lower right")
  plt.show()
}


df = data.frame(age=c(123,3,5,2,3,5,6,3,2,NA), thing=c(3,5,5,2, NA, NA, 123, 5,2,3), other=c(NA,NA,NA,NA,NA,NA,1,2,2))
