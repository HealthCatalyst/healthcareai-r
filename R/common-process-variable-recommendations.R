processVariableDfForOneRow <- function(baseRow,
                                       modifiableProcessVariables,
                                       factorLevels,
                                       predictFunction) {
  
  currentPrediction <- as.numeric(predictFunction(baseRow))
  
  listOfSingleRowDfs <- list()
  
  for (processVar in modifiableProcessVariables) {
    
    currentValue <- baseRow[[processVar]]
    levels <- factorLevels[[processVar]]
    
    for (level in levels) {
      modifiedRow <- baseRow
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
  
  return(processVarDf)
}


