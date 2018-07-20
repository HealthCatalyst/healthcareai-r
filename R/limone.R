pip <- function(model, d, new_values, repeated_factors = FALSE,
                number_of_factors = 3, smaller_better = TRUE, expected_signs, id) {

  id <- rlang::enquo(id)
  # Try to find an ID column in the model's recipe
  if (rlang::quo_is_missing(id))
    id <- rlang::as_quosure(attr(attr(model, "recipe"), "ignored_columns"))

  # Add row_id column used to join permutations; deleted at end
  d <- dplyr::mutate(d, row_id = dplyr::row_number())

  # Build big dataframe of permuted data
  permuted_df <- permute_process_variables(d, new_values)

  # Make Predictions on the original and permuted data
  suppressWarnings({
    suppressMessages({
      d <- predict(model, newdata = d)
      names(d)[stringr::str_which(names(d), "^predicted_")[1]] <- "base_prediction"
      permuted_df <- predict(model, newdata = permuted_df)
      names(permuted_df)[stringr::str_which(names(permuted_df), "^predicted_")[1]] <- "new_prediction"
    })
  })

  # Scaling constant to change the ordering depending on smaller_better
  ordering_direction <- if (smaller_better) -1 else 1

  # Join the dataframes containing the old and new predictions
  d <-
    d %>%
    select(!!id, row_id, base_prediction) %>%
    # Join on row number
    dplyr::inner_join(
      select(permuted_df, row_id, new_prediction, current_value,
             alt_value, process_variable_name)
      , by = "row_id") %>%
    # Add delta column
    dplyr::mutate(improvement = new_prediction - base_prediction)

  # Check for predictions that are same or worse as the original and
  # replace the recomendation and modified prediction with the originals
  # and the delta with 0
  if (smaller_better) {
    to_fix <- d$improvement >= 0L
    d$improvement <- d$improvement * -1
  } else {
    to_fix <- d$improvement <= 0L
  }

  if (length(to_fix)) {
    # These rows will only be exposed if best delta is 0, so replace with current
    d$alt_value[to_fix] <- d$current_value[to_fix]
    d$new_prediction[to_fix] <- d$base_prediction[to_fix]
    d$improvement[to_fix] <- 0
  }

  d %>%
    select(!!id, row_id,
           modifiable_variable = process_variable_name,
           original_value = current_value,
           modified_value = alt_value,
           original_prediction = base_prediction,
           modified_prediction = new_prediction,
           improvement) %>%
    group_by(row_id) %>%
    arrange(row_id, desc(improvement)) %>%
    mutate(value_rank = dplyr::row_number()) %>%
    filter(value_rank <= number_of_factors) %>%
    ungroup() %>%
    select(-row_id, -value_rank)
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
build_process_variables_df <- function(list_of_dataframes,
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
  full_df <-
    do.call(rbind, lapply(list_of_dataframes, function(row_df){
      # Start by including the grain coulumn
      cbind(row_id = row_df$row_id[[1]],
            # Combine first few rows into a single row with the grain
            do.call(cbind,
                    lapply(seq_len(number_of_factors), function(i) {

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
    row_df[!duplicated(row_df[, c(column_name)]), , drop = FALSE]  # nolint
  })
}
