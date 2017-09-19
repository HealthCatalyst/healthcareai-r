build_process_variable_df_list <- function(dataframe,
                                           grainColumnValues,
                                           modifiable_variable_levels,
                                           predict_function, 
                                           low_probabilities_desired) {
  # Add the grain colum
  dataframe["df_grain_column"] <- grainColumnValues
  # Build big dataframe of permuted data
  permuted_df <- permute_process_variables(dataframe,
                                           modifiable_variable_levels)

  # Columns not used in prediction
  tracking_columns <- c("df_grain_column", "process_variable_name", "alt_value")
  
  # Make Predictions on the original and permuted data
  dataframe$base_prediction <- predict_function(newData = dataframe[!names(dataframe) %in% tracking_columns])
  permuted_df$new_prediction <- predict_function(newData = permuted_df[!names(permuted_df) %in% tracking_columns])
  
  # Scaling constant to change the ordering depending on 
  # low_probabilities_desired; takes values +/- 1
  ordering_direction <- (-1)^low_probabilities_desired
    
  # Join the dataframes containing the old and new predictions
  full_df <- dataframe %>%
    # Join on row number
    dplyr::inner_join(permuted_df, by = "df_grain_column") %>%
    # Add delta column
    dplyr::mutate(delta = new_prediction - base_prediction) %>% 
    # Restrict to desired columns
    dplyr::select(df_grain_column,
                  process_variable_name, 
                  current_value, 
                  alt_value, 
                  base_prediction, 
                  new_prediction, 
                  delta) %>%
    # For each grain column id, order the results by delta
    dplyr::arrange(df_grain_column, ordering_direction*desc(delta))

  # Split the large dataframe into a list of dataframes
  split(full_df, as.factor(full_df$df_grain_column))
}

permute_process_variables <- function(dataframe,
                                      modifiable_variable_levels) {
  modifiable_names <- names(modifiable_variable_levels)

  # Build a dataframe for each modifiable variable and glue these together
  dplyr::bind_rows(lapply(seq_along(modifiable_variable_levels), function(i) {
    # Get variable name and levels
    modifiable_variable <- modifiable_names[i]
    levels <- factor(modifiable_variable_levels[[i]])

    # For one variable, cycle through all levels and build a dataframe for each
    # one by perturbing the levels, then combine these.
    one_variable_df <- dplyr::bind_rows(lapply(levels,
                                               FUN = build_one_level_df,
                                               dataframe = dataframe,
                                               modifiable_variable = modifiable_variable))
    # Add the modifiable variable to the dataframe
    one_variable_df["process_variable_name"] <- as.character(modifiable_variable)
    # 
    one_variable_df
  }))
}

build_one_level_df <- function(dataframe, modifiable_variable, level) {
  # Add reference columns
  dataframe["current_value"] <- as.character(dataframe[[modifiable_variable]])
  dataframe["alt_value"] <- as.character(level)
  # Fill the modifiable variable column with the specified level
  dataframe[[modifiable_variable]] <- level
  return(dataframe)
}


build_process_variables_df = function(modFactorsList,
                                      repeatedFactors = FALSE,
                                      numTopFactors = 3) {
  # Drop repeated rows, if desired
  if (!repeatedFactors) {
    modFactorsList <- drop_repeated(df_list = modFactorsList, 
                                    column_name = "process_variable_name")
  }
  
  # Determine the maximum number of modifiable factors
  numTopFactors <- min(numTopFactors, nrow(modFactorsList[[1]]))
  # Extract the first few rows from each individual dataframe and combine 
  # them into one long row. Build the full dataframe out of such long rows.
  modFactorsDf <- do.call(rbind, lapply(modFactorsList, function(rowDf){
    # Start by including the grain coulumn
    cbind(df_grain_column = rowDf$df_grain_column[[1]],
          # Combine first few rows into a single row with the grain
          do.call(cbind, lapply(1:numTopFactors, function(i) {
            # Drop unwanted columns
            row <- rowDf[i, !(names(rowDf) %in% c("df_grain_column",
                                                  "base_prediction"))]
            # Rename the columns
            names(row) <- c(paste0("Modify", i, "TXT"),
                            paste0("Modify", i, "Current"), 
                            paste0("Modify", i, "AltValue"),
                            paste0("Modify", i, "AltPrediction"),
                            paste0("Modify", i, "Delta"))
            # Return the long row to help build the full dataframe
            row
          }))
    )
  }))
  row.names(modFactorsDf) <- NULL
  
  return(modFactorsDf)
}


#' @title
#' Simultaneously remove duplicate row values in a list of dataframes.
#' @description Takes a list of dataframes, each of which contains a column 
#' with the same name and, for each dataframe, removes rows where the value in 
#' the common column has already occured in a previous row of that dataframe.
#' @param df_list A list of dataframes. Each dataframe should contain a column
#' whose name matches \code{column_name}
#' @param column_name The name of the column from which to remove duplicates.
#' @return a list of dataframes with no repeated values in \code{column_name}
#' @keywords internal
drop_repeated <- function(df_list, column_name) {
  # Build and return a list of trimmed dataframes
  lapply(df_list, function(row_df) {
    # Drop duplicates
    row_df[!duplicated(row_df[, c(column_name)]), ]
  })
}