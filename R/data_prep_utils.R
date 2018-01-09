
find_0_1_cols <- function(d) {
  # Returns names of columns that have only 0/1 in them.

}

find_date_cols <- function(d) {
  cols <- c(
    names(d[purrr::map(d, is.Date) == TRUE]),
    names(d[purrr::map(d, is.POSIXct) == TRUE]),
    names(d[grepl(pattern = "DTS", names(d))])
  )
  cols <- unique(cols)
  return(cols)
}
