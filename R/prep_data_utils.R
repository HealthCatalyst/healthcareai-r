find_0_1_cols <- function(d) {
  # Returns names of columns that have only 0/1 in them.
  cols <- purrr::map_lgl(d, ~ all(.x %in% c(0L, 1L, NA_integer_)))
  return(names(d)[cols])
}

find_mostly_missing_cols <- function(d, percent_missing_threshold = 80) {
  # Finds columns with more missing values than the threshold
  cols <-
    missingness(d) %>%
    dplyr::filter(percent_missing > percent_missing_threshold)
  return(cols$variable)
}

find_date_cols <- function(d) {
  # Returns names of date columns
  by_class <-
    purrr::map_lgl(d, ~ lubridate::is.Date(.x) || lubridate::is.POSIXt(.x))
  by_name <- purrr::map_lgl(names(d), ~grepl("DTS$", .x))
  return(names(d)[by_class | by_name])
}
