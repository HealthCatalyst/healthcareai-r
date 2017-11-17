#' pivot
#'
#' @param d data frame
#' @param grain Column that defines rows. Unquoted.
#' @param spread Column that will become multiple columns. Unquoted.
#' @param fill Column to be used to fill the values of cells in the output (
#' perhaps after aggregation by \code{fun}). If \code{fill} is not provided,
#' counts will be used, as though a fill column of 1s had been provided.
#' @param fun Function for aggreation, defaults to \code{sum}.
#' @param missing_fill Value to fill for combinations of grain and spread that
#' are not present. Defaults to NA, but 0 may be useful as well.
#'
#' @return A tibble data frame with one row for each unique value of
#' \code{grain}, and one column for each unique value of \code{spread} plus
#' one column for grain.
#'
#' @details Entries in the tibble are defined by the fill column. Combinations of
#' \code{grain} x \code{spread} that are not present in \code{d} will be filled
#' in with \code{missing_fill}. If there are \code{grain} x \code{spread} pairs
#' that appear more than once in d, they will be aggregated by \code{fun}.
#'
#' @importFrom rlang :=
#' @export
#'
#' @examples
#'
pivot <- function(d, grain, spread, fill, fun = sum, missing_fill = NA) {
  # pivot "spreads" spread into separate columns, creating one row for
  # each entry in grain
  # fill can be the name of a column in d containing value to fill in
  # otherwise it should be a single value that value will be used to fill in
  # all present values

  # Capture argument column names as quosures
  grain <- rlang::enquo(grain)
  spread <- rlang::enquo(spread)
  fill <- rlang::enquo(fill)

  # If fill wasn't provided, create and use a column with all 1s, with message
  if (rlang::quo_is_missing(fill)) {
    message("No fill column was provided, so using \"1\" for present entities")
    # Create quosure'd variable name "fill_ones"
    fill <- rlang::quo(fill_ones)
    d <- dplyr::mutate(d, !!rlang::quo_name(fill) := rep(1L, nrow(d)))
  }

  # Make sure all three columns are present
  cols <- sapply(c(grain, spread, fill), rlang::quo_name)
  present <- cols %in% names(d)
  if (any(!present))
    stop(paste(cols[!present], collapse = ", "),
         " not found in ", match.call()$d)

  # Make sure there's no missingness in grain or spread
  missing_check(d, grain)
  missing_check(d, spread)

  # Make sure fun is a function
  if (!is.function(fun)) stop(match.call()$fun, "isn't a function")

  # Convert grouping variables to factors
  d <-
    d %>%
    dplyr::mutate(!!rlang::quo_name(grain) := as.factor(!!grain),
                  !!rlang::quo_name(spread) := as.factor(!!spread))

  # Check if there are any grain-spread pairs that have more than one entry...
  need_aggregate <- any(duplicated(dplyr::select(d, !!grain, !!spread)))
  # ... If there are, aggregate rows
  if (need_aggregate) {
    # Test if the user provided a 'fun'. If not warn that we'll use sum
    if (is.null(match.call()$fun))
      message("There are rows that contain the same values of both ",
              rlang::get_expr(grain), " and ", rlang::get_expr(spread),
              " but you didn't provide a function for their aggregation. ",
              "Proceding with the default: fun = sum.")
    d <- aggregate_rows(d, grain, spread, fill, fun)
  }

  out <- pivot_maker(d, grain, spread, fill, missing_fill)

  return(out)
}

#' Aggregate rows
#'
#' @param d
#' @param grain
#' @param spread
#' @param fill
#' @param fun
#'
#' @details All variables comes straight through from pivot.
#'
#' @return Aggregated data frame
#' @noRd
aggregate_rows <- function(d, grain, spread, fill, fun) {
  d <-
    d %>%
    dplyr::group_by(!!grain, !!spread) %>%
    dplyr::summarize(!!rlang::quo_name(fill) := fun(!!fill)) %>%
    dplyr::ungroup()

  return(d)
}

#' Make pivoted table
#'
#' @param d
#' @param grain
#' @param spread
#' @param fill
#' @param missing_fill
#' @details All variables come through from pivot
#' @return Pivoted tibble. One row for each grain; one column for each spread
#' @noRd
pivot_maker <- function(d, grain, spread, fill, missing_fill) {
  # Create a data frame of missing_fill, to be replaced where appropriate.
  # Critically, columns are arranged by the integer representation of spread,
  # which allows us to index the entries by those integers.

  # Pull factor levels to use as row deliniators and column names
  to_rows <- levels(dplyr::pull(d, !!grain))
  to_cols <- levels(dplyr::pull(d, !!spread))

  pivoted <-
    matrix(missing_fill,
           nrow = length(to_rows), ncol = length(to_cols),
           dimnames = list(NULL, to_cols)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(!!rlang::quo_name(grain) := to_rows)

  pivoted <-
    # Loop over each entry in grain
    purrr::map_df(to_rows,  ~ {
      # Pull the relevant row of interest
      keep <- dplyr::filter(pivoted, (!!grain) == .x)
      # Inside-out:
      # i IDs rows in original dataframe that we want in this row of output
      # Subsetting d$spread by those rows gives us factor levels to fill in.
      # Converting them to integers gives the indices to fill in.
      i <- which(dplyr::pull(d, !!grain) == .x)
      keep[as.integer(dplyr::pull(d, !!spread)[i])] <- dplyr::pull(d, !!fill)[i]
      return(keep)
    })

  # Arrange and rename columns
  ## Put the grain column first
  pivoted <- pivoted[, c(ncol(pivoted), 1:(ncol(pivoted) - 1))]
  ## Add spread as prefix to nonID columns
  names(pivoted)[2:ncol(pivoted)] <-
    paste0(rlang::quo_name(spread), "_", names(pivoted)[2:ncol(pivoted)])

  return(pivoted)
}
