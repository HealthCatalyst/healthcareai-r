build_process_df <- function(dataframe,
                             modifiable_variable_levels) {
  modifiable_names <- names(modifiable_variable_levels)

  # Split up by variable
  
  # Copy for each level
  do.call(rbind, lapply(seq_along(modifiable_variable_levels), function(i) {
    # Fix variable
    modifiable_variable <- modifiable_names[i]
    levels <- modifiable_variable_levels[[i]]
    # For one variable, cycle through all levels
    one_variable_df <- do.call(rbind, lapply(seq_along(levels), function(j) {
      df <- dataframe
      # Replace modifiable variable with level
      level <- levels[[j]]
      df[[modifiable_variable]] <- level
      df["alt_value"] <- as.character(level)
      # Return the modified dataframe
      df
    }))
    one_variable_df["process_variable_name"] <- as.character(modifiable_variable)
    one_variable_df
  }))
}


# One row of data
processVariableDfForOneRow <- function(baseRow,
                                       modifiableProcessVariables,
                                       factorLevels,
                                       predictFunction,
                                       largerPredictionsDesired) {
  
  # get prediction for unmodified row
  currentPrediction <- as.numeric(predictFunction(baseRow))
  
  # list of 1-row dataframes, one for each modifiable process variable
  listOfSingleRowDfs <- list()
  
  for (processVar in modifiableProcessVariables) {
    
    # get the current value
    currentValue <- baseRow[[processVar]]
    # get the factor levels for that variable
    levels <- factorLevels[[processVar]]
    
    for (level in levels) {
      modifiedRow <- baseRow
      # modify the row
      modifiedRow[[processVar]] <- factor(level, levels = levels)
      
      newPrediction <- as.numeric(predictFunction(modifiedRow))
      delta <- newPrediction - currentPrediction
      
      summaryDf <- data.frame(variable = as.character(processVar),
                              currentValue = as.character(currentValue),
                              altValue = as.character(level),
                              altPrediction = newPrediction,
                              delta = delta)
      
      listOfSingleRowDfs[[paste0(processVar, '.', level)]] <- summaryDf
    }
  }
  processVarDf <- do.call(rbind, listOfSingleRowDfs)
  processVarDf <- processVarDf[order(processVarDf$delta,
                                     decreasing = largerPredictionsDesired), ]
  
  return(processVarDf)
}

buildProcessVariableDfList <- function(dataframe,
                                       modifiableProcessVariables,
                                       factorLevels,
                                       predictFunction,
                                       largerPredictionsDesired) {
  
  currentPredictions <- as.numeric(predictFunction(dataframe))
  
  listOfLists <- list()
  for (i in 1:nrow(dataframe)) {
    listOfLists[[i]] <- list()
  }
  
  for (processVar in modifiableProcessVariables) {
    
    currentValues <- dataframe[[processVar]]
    levels <- factorLevels[[processVar]]
    
    for (level in levels) {
      modifiedDf <- dataframe
      modifiedDf[[processVar]] <- factor(level, levels = levels)
      
      newPredictions <- as.numeric(predictFunction(modifiedDf))
      deltas <- newPredictions - currentPredictions
      
      summaryDf <- data.frame(variable = as.character(processVar),
                              currentValue = as.character(currentValues),
                              altValue = as.character(level),
                              altPrediction = newPredictions,
                              delta = deltas)
      for (i in 1:nrow(dataframe)) {
        listOfLists[[i]][[paste0(processVar, '.', level)]] <- summaryDf[i, ]
      }
    }
  }
  listOfDfs <- list()
  for (i in 1:nrow(dataframe)) {
    processVarDf <- do.call(rbind, listOfLists[[i]])
    listOfDfs[[i]] <- processVarDf[order(processVarDf$delta,
                                         decreasing = largerPredictionsDesired), ]
  }
  return(listOfDfs)
}

buildProcessVariablesDf = function(modFactorsList,
                                   repeatedFactors = FALSE,
                                   numTopFactors = 3) {
  # Drop repeated rows, if desired
  if (!repeatedFactors) {
    modFactorsList <- dropRepeated(dfList = modFactorsList, 
                                   columnName = "variable")
  }
  
  # Aetermine the maximum number of modifiable factors
  numTopFactors <- min(numTopFactors, nrow(modFactorsList[[1]]))
  # Combine into dataframe
  modFactorsDf <- do.call(rbind, lapply(modFactorsList, function(rowDf){
    do.call(cbind, lapply(1:numTopFactors, function(i) {
      row <- rowDf[i, ]
      names(row) <- c(paste0("Modify", i, "TXT"),
                      paste0("Modify", i, "Current"), 
                      paste0("Modify", i, "AltValue"),
                      paste0("Modify", i, "AltPrediction"),
                      paste0("Modify", i, "Delta"))
      return(row)
    }))
  }))
  row.names(modFactorsDf) <- NULL
  
  return(modFactorsDf)
}

drop_repeated <- function(df_list, column_name) {
  # Build and return a list of trimmed dataframes
  lapply(df_list, function(row_df) {
    # Drop duplicates
    row_df[!duplicated(row_df[, c(column_name)]), ]
  })
}
