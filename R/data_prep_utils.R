
find_0_1_cols <- function(d) {
  # Returns names of columns that have only 0/1 in them.
  d <- d[purrr::map(d, is.numeric) == TRUE]
  cols <- purrr::map(d, function(x) {
    n <- names(table(x))
    ifelse(all(n[ordered(n)][1:2] == c("0", "1")),
           TRUE,
           FALSE)})
  cols <- names(cols[cols == TRUE])
  return(cols)
}

find_mostly_missing_cols <- function(d, percent_missing_threshold = 80) {
  # Finds columns with more missing values than the threshold
  cols <- missingness(d) %>%
    dplyr::filter(percent_missing > percent_missing_threshold)
  return(cols$variable)
}


find_date_cols <- function(d) {
  # Returns names of date columns
  cols <- c(
    names(d[purrr::map(d, lubridate::is.Date) == TRUE]),
    names(d[purrr::map(d, lubridate::is.POSIXct) == TRUE]),
    names(d[grepl(pattern = "DTS", names(d))])
  )
  cols <- unique(cols)
  return(cols)
}

