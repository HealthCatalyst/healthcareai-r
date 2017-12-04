#' Pivot from a long data frame to a wide data frame
#'
#' @param d data frame
#' @param grain Column that defines rows. Unquoted.
#' @param spread Column that will become multiple columns. Unquoted.
#' @param fill Column to be used to fill the values of cells in the output,
#' perhaps after aggregation by \code{fun}. If \code{fill} is not provided,
#' counts will be used, as though a fill column of 1s had been provided.
#' @param fun Function for aggregation, defaults to \code{sum}. Custom functions
#' can be used with the same syntax as the apply family of functions, e.g.
#' \code{fun = function(x) some_function(another_fun(x))}.
#' @param missing_fill Value to fill for combinations of grain and spread that
#' are not present. Defaults to NA, but 0 may be useful as well.
#'
#' @return A tibble data frame with one row for each unique value of
#' \code{grain}, and one column for each unique value of \code{spread} plus
#' one column for the entries in grain.
#'
#' Entries in the tibble are defined by the fill column. Combinations of
#' \code{grain} x \code{spread} that are not present in \code{d} will be filled
#' in with \code{missing_fill}. If there are \code{grain} x \code{spread} pairs
#' that appear more than once in d, they will be aggregated by \code{fun}.
#'
#' @details \code{pivot} is useful when you want to change the grain of your
#' data, for example from the procedure grain to the patient grain. In that
#' example, each patient might have 0, 1, or more medications. To make a
#' patient-level table, we need a column for each medication, which is what
#' it means to make a wide table. The \code{fill} argument dictates what to
#' put in each of the medication columns, e.g. the dose the patient got.
#' \code{fill} defaults to "1", as an indicator variable. If any patients have
#' multiple rows for the same medication (say they recieved a med more than
#' once), we need a way to deal with that, which is what the \code{fun} argument
#' handles. By default it uses \code{sum}, so if \code{fill} is left as its
#' default, the count of instances for each patient will be used.
#'
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' meds <-
#'   tibble::tibble(
#'     patient_id = c("A", "A", "A", "B"),
#'     medication = c("zoloft", "asprin", "lipitor", "asprin"),
#'     pills_per_day = c(1, 8, 2, 4)
#'   )
#' meds
#'
#' # Number of pills of each medication each patient gets:
#' pivot(
#'   d = meds,
#'   grain = patient_id,
#'   spread = medication,
#'   fill = pills_per_day,
#'   missing_fill = 0
#' )
#'
#' bills <-
#'   tibble::tibble(
#'     patient_id = rep(c("A", "B"), each = 4),
#'     dept_id = rep(c("ED", "ICU"), times = 4),
#'     charge = runif(8, 0, 1e4),
#'     date = Sys.Date() - sample(0:2, 8, TRUE)
#'   )
#' bills
#'
#' # Total charges per patient x department:
#' pivot(bills, patient_id, dept_id, charge, sum)
#'
#' # Count of charges per patient x day:
#' pivot(bills, patient_id, date)
#'
#' # Can provide a custom function to fun, which will take fill as input.
#' # Get the difference between the greatest and smallest charge in each
#' # department for each patient and format it as currency.
#' pivot(d = bills,
#'       grain = patient_id,
#'       spread = dept_id,
#'       fill = charge,
#'       fun = function(x) paste0("$", round(max(x) - min(x), 2))
#' )
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
              "Proceeding with the default: fun = sum.")
    d <- aggregate_rows(d, grain, spread, fill, fun)
  }

  out <- pivot_maker(d, grain, spread, fill, missing_fill)

  return(out)
}

#' Aggregate rows
#' @details All variables come through from pivot
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
