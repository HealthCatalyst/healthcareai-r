library(tidyverse)

build_process_variable_df_list <- function(dataframe,
                                           modifiable_variable_levels,
                                           predict_function, 
                                           low_probabilities_desired) {
  # Add row numbers to 
  dataframe["df_row_number"] <- 1:nrow(dataframe)
  # Build big dataframe of permuted data
  permuted_df <- permute_process_variables(dataframe,
                                           modifiable_variable_levels)
  
  tracking_columns <- c("df_row_number", "process_variable_name", "alt_value")
  
  # Make Predictions 
  dataframe$base_prediction <- predict_function(newData = dataframe[!names(dataframe) %in% tracking_columns])
  permuted_df$new_prediction <- predict_function(newData = permuted_df[!names(permuted_df) %in% tracking_columns])

    
  full_df <- dataframe %>%
    # Join on row number
    dplyr::inner_join(permuted_df, by = "df_row_number") %>%
    # Add delta column
    dplyr::mutate(delta = new_prediction - base_prediction) %>% 
    # Restrict to desired columns
    dplyr::select(df_row_number,
                  process_variable_name, 
                  current_value, 
                  alt_value, 
                  base_prediction, 
                  new_prediction, 
                  delta)
  
  ordering_direction <- (-1)^low_probabilities_desired
  
  lapply(1:nrow(dataframe), function(i) {
    full_df %>%
      dplyr::filter(df_row_number == i) %>%
      dplyr::arrange(ordering_direction*desc(delta)) %>%
      dplyr::select(-c(df_row_number))
  })
}

permute_process_variables <- function(dataframe,
                                      modifiable_variable_levels) {
  modifiable_names <- names(modifiable_variable_levels)

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
      df["current_value"] <- as.character(dataframe[[modifiable_variable]])
      df["alt_value"] <- as.character(level)
      # Return the modified dataframe
      df
    }))
    one_variable_df["process_variable_name"] <- as.character(modifiable_variable)
    # 
    one_variable_df
  }))
}


build_process_variables_df = function(modFactorsList,
                                      repeatedFactors = FALSE,
                                      numTopFactors = 3) {
  # Drop repeated rows, if desired
  if (!repeatedFactors) {
    modFactorsList <- drop_repeated(df_list = modFactorsList, 
                                    column_name = "process_variable_name")
  }
  
  # Aetermine the maximum number of modifiable factors
  numTopFactors <- min(numTopFactors, nrow(modFactorsList[[1]]))
  # Combine into dataframe
  modFactorsDf <- do.call(rbind, lapply(modFactorsList, function(rowDf){
    do.call(cbind, lapply(1:numTopFactors, function(i) {
      row <- rowDf[i, !(names(rowDf) %in% c("base_prediction"))]
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