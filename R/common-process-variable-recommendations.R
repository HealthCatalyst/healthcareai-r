# Needed to avoid a CMD check note
# TODO: move to a better place
globalVariables(c(".data"))

# 1. Build List of Dataframes --------------------------------------------------
# The following use a dataframe of (deploy) data to build a list of dataframes,
# one for each row of data, containing information about how modifying each
# modifiable process variable affects the prediction.

#' @title
#' Build a list of dataframes with new predictions for each modifiable variable.
#' @description Builds a list consisting of one dataframe per row in
#' \code{dataframe} which shows how the predictions for that row change as
#' each of the mofifiable variables are altered
#' @param dataframe A dataframe consisting of deployment data.
#' @param grain_column_values The grain column (values, not just the name) of 
#' the deployment data.
#' @param modifiable_variable_levels A list indexed by the modifiable 
#' process variables and containing the factor levels of each such variable.
#' @param predict_function A function with which to make new predictions on the
#' data.
#' @param smaller_better A boolean indicating whether the goal is to
#' increase or decrease the predictions/predicted probabilities.
#' @return A list of dataframes.
#' @keywords internal
#' @importFrom dplyr %>% desc
build_process_variable_df_list <- function(dataframe,
                                           grain_column_values,
                                           modifiable_variable_levels,
                                           predict_function, 
                                           smaller_better) {

  # Add the grain colum
  dataframe["df_grain_column"] <- grain_column_values
  # Build big dataframe of permuted data
  permuted_df <- permute_process_variables(dataframe,
                                           modifiable_variable_levels)

  # Columns not used in prediction
  tracking_columns <- c("df_grain_column", "process_variable_name", "alt_value")
  
  # Make Predictions on the original and permuted data
  dataframe$base_prediction <- predict_function(newData = dataframe[!names(dataframe) %in% tracking_columns])
  permuted_df$new_prediction <- predict_function(newData = permuted_df[!names(permuted_df) %in% tracking_columns])
  
  # Scaling constant to change the ordering depending on smaller_better
  ordering_direction <- if (smaller_better) -1 else 1 
    
  # Join the dataframes containing the old and new predictions
  full_df <- dataframe %>%
    # Join on row number
    dplyr::inner_join(permuted_df, by = "df_grain_column") %>%
    # Add delta column
    dplyr::mutate(delta = .data[["new_prediction"]] - 
                    .data[["base_prediction"]]) %>% 
    # Restrict to desired columns
    dplyr::select(.data[["df_grain_column"]],
                  .data[["process_variable_name"]], 
                  .data[["current_value"]], 
                  .data[["alt_value"]], 
                  .data[["base_prediction"]], 
                  .data[["new_prediction"]], 
                  .data[["delta"]]) %>%
    # For each grain column id, order the results by delta
    dplyr::arrange(.data[["df_grain_column"]], 
                   ordering_direction*desc(.data[["delta"]]))
  
  # Check for predictions that are same or worse as the original and       
  # replace the recomendation and modified prediction with the originals
  # and the delta with 0
  to_fix <-
    if (smaller_better) {
      full_df$delta >= 0L
    } else {
      full_df$delta <= 0L
    }

  if (length(to_fix)) {
    # These rows will only be exposed if best delta is 0, so replace with current
    full_df$alt_value[to_fix] <- full_df$current_value[to_fix]
    full_df$new_prediction[to_fix] <- full_df$base_prediction[to_fix]
    full_df$delta[to_fix] <- 0
  }

  # Split the large dataframe into a list of dataframes
  split(full_df, as.factor(full_df$df_grain_column))
}

#' @title Take a dataframe and build a larger dataframe by permuting the 
#' values in certain columns.
#' @description Take a dataframe and build a larger dataframe by permuting the 
#' values in the columns corresponding to \code{modifiable_variable_levels}.
#' @param dataframe A dataframe.
#' @param modifiable_variable_levels A list indexed by the modifiable 
#' process variables and containing the factor levels of each such variable.
#' @return A large dataframe where the values in the modifiable variable 
#' columns have been permuted.
#' @keywords internal
#' @importFrom dplyr %>%
permute_process_variables <- function(dataframe,
                                      modifiable_variable_levels) {
  # Get the names of the modifiable variables
  modifiable_names <- names(modifiable_variable_levels)

  # Build a dataframe for each modifiable variable and glue these together
  lapply(X = seq_along(modifiable_variable_levels), FUN = function(i) {
    # Get variable name and levels
    modifiable_variable <- modifiable_names[i]
    if (is.factor(dataframe[[modifiable_variable]])) {
      levels <- factor(modifiable_variable_levels[[i]], 
                       levels = levels(dataframe[[modifiable_variable]]))
    } else {
      levels <- modifiable_variable_levels[[i]]
    }

    # For one variable, cycle through all levels and build a dataframe for
    # each one by perturbing the levels, then combine these.
    one_variable_df <- lapply(X = levels,
                              FUN = build_one_level_df,
                              dataframe = dataframe,
                              modifiable_variable = modifiable_variable) %>%
      dplyr::bind_rows()
    # Add the modifiable variable to the dataframe
    one_variable_df["process_variable_name"] <- as.character(modifiable_variable)
    # Output each one_variable_df so that they may be combined
    one_variable_df
  }) %>% 
    dplyr::bind_rows()
}


#' @title
#' Replace all value in the column of a dataframe with a given value.
#' @description Takes a dataframe and replaces all the values in the 
#' \code{modifiable_variable} column with the value \code{level}. Columns
#' tracking which variable and level were used are also added to the dataframe.
#' @param dataframe a dataframe
#' @param modifiable_variable The name of the column whose values we wish to
#' change
#' @param level The value to use in the \code{modifiable_variable} column
#' @return A dataframe
#' @keywords internal
build_one_level_df <- function(dataframe, modifiable_variable, level) {
  # Add reference columns
  dataframe$current_value <- as.character(dataframe[[modifiable_variable]])
  dataframe$alt_value <- as.character(level)
  # Fill the modifiable variable column with the specified level
  dataframe[[modifiable_variable]] <- level
  return(dataframe)
}

# 2. Build Output Dataframe ----------------------------------------------------
# The folloiwng functions take the list of dataframes generated using the 
# functions in 1. and combine these into a single dataframe with one row for
# each original row of deploy data. This dataframe provides the top n most
# useful modifiable factors.

#' @title 
#' Build a the output dataframe for modifiable process variables from a list
#' of dataframes.
#' @description Takes a list of dataframes built using 
#' \code{build_process_variable_df_list} and returns a single dataframe listing 
#' the best modifiable process variables to modify.
#' @param list_of_dataframes A list of dataframes built using 
#' \code{build_process_variable_df_list}.
#' @param repeated_factors A boolean determining whether or not a single 
#' modifiable factor can be listed several times.
#' @param number_of_factors The number of modifiable process variables to 
#' include in each row.
#' @return A dataframe.
#' @keywords internal
build_process_variables_df = function(list_of_dataframes,
                                      repeated_factors = FALSE,
                                      number_of_factors = 3) {
  # Drop repeated rows, if desired
  if (!repeated_factors) {
    list_of_dataframes <- drop_repeated(df_list = list_of_dataframes, 
                                    column_name = "process_variable_name")
  }
  
  # Determine the maximum number of modifiable factors
  number_of_factors <- min(number_of_factors, nrow(list_of_dataframes[[1]]))
  
  # Extract the first few rows from each individual dataframe and combine 
  # them into one long row. Build the full dataframe out of such long rows.
  full_df <- do.call(rbind, lapply(list_of_dataframes, function(row_df){
    # Start by including the grain coulumn
    cbind(df_grain_column = row_df$df_grain_column[[1]],
          # Combine first few rows into a single row with the grain
          do.call(cbind, lapply(1:number_of_factors, function(i) {
            # Drop unwanted columns
            row <- row_df[i, !(names(row_df) %in% c("df_grain_column",
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
  row.names(full_df) <- NULL
  
  return(full_df)
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
    row_df[!duplicated(row_df[, c(column_name)]), , drop = FALSE]
  })
}