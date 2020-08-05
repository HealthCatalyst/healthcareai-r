#' Pivot multiple rows per observation to one row with multiple columns
#'
#' @param d data frame
#' @param grain Column that defines rows. Unquoted.
#' @param spread Column that will become multiple columns. Unquoted.
#' @param fill Column to be used to fill the values of cells in the output,
#'   perhaps after aggregation by \code{fun}. If \code{fill} is not provided,
#'   counts will be used, as though a fill column of 1s had been provided.
#' @param fun Function for aggregation, defaults to \code{sum}. Custom functions
#'   can be used with the same syntax as the apply family of functions, e.g.
#'   \code{fun = function(x) some_function(another_fun(x))}.
#' @param missing_fill Value to fill for combinations of grain and spread that
#'   are not present. Defaults to NA, but 0 may be useful as well.
#' @param extra_cols Values of \code{spread} to create all-\code{missing_fill}
#'   columns, for e.g. if you want to add levels that were observed in training
#'   but are not present in deployment.
#'
#' @return A tibble data frame with one row for each unique value of
#'   \code{grain}, and one column for each unique value of \code{spread} plus
#'   one column for the entries in grain.
#'
#'   Entries in the tibble are defined by the fill column. Combinations of
#'   \code{grain} x \code{spread} that are not present in \code{d} will be
#'   filled in with \code{missing_fill}. If there are \code{grain} x
#'   \code{spread} pairs that appear more than once in d, they will be
#'   aggregated by \code{fun}.
#'
#' @details \code{pivot} is useful when you want to change the grain of your
#'   data, for example from the procedure grain to the patient grain. In that
#'   example, each patient might have 0, 1, or more medications. To make a
#'   patient-level table, we need a column for each medication, which is what it
#'   means to make a wide table. The \code{fill} argument dictates what to put
#'   in each of the medication columns, e.g. the dose the patient got.
#'   \code{fill} defaults to "1", as an indicator variable. If any patients have
#'   multiple rows for the same medication (say they recieved a med more than
#'   once), we need a way to deal with that, which is what the \code{fun}
#'   argument handles. By default it uses \code{sum}, so if \code{fill} is left
#'   as its default, the count of instances for each patient will be used.
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
#'     date = as.Date("2024-12-25") - sample(0:2, 8, TRUE)
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
pivot <- function(d, grain, spread, fill, fun = sum, missing_fill = NA, extra_cols) {
  # pivot "spreads" spread into separate columns, creating one row for
  # each entry in grain
  # fill can be the name of a column in d containing value to fill in
  # otherwise it should be a single value that value will be used to fill in
  # all present values

  # Capture argument column names as quosures
  grain <- rlang::enquo(grain)
  spread <- rlang::enquo(spread)
  fill <- rlang::enquo(fill)
  # Check if the user provided a function for warning later
  default_fun <- is.null(match.call()$fun)

  # If fill wasn't provided, create and use a column with all 1s, with message
  if (rlang::quo_is_missing(fill)) {
    message("No fill column was provided, so using \"1\" for present entities")
    # Create quosure'd variable name "fill_ones"
    fill <- rlang::quo(fill_ones)
    d <- dplyr::mutate(d, !!rlang::quo_name(fill) := rep(1L, nrow(d)))
  }

  # Make sure all three columns are present
  cols <- purrr::map_chr(c(grain, spread, fill), rlang::quo_name)
  present <- cols %in% names(d)
  if (any(!present))
    stop(list_variables(cols[!present]), " not found in d.")

  # Make sure there's no missingness in grain or spread
  missing_check(d, grain)
  missing_check(d, spread)

  # Make sure fun is a function
  if (!is.function(fun)) stop("fun isn't a function")

  d <- do_aggregate(d, grain, spread, fill, fun, default_fun)

  out <- pivot_maker(d, grain, spread, fill, missing_fill)

  # Add extra columns
  if (!missing(extra_cols)) {
    extra_cols <- paste0(quo_name(spread), "_", extra_cols)
    class(missing_fill) <- class(d[[rlang::quo_name(fill)]])
    out <-
      matrix(missing_fill,
             nrow = nrow(out), ncol = length(extra_cols),
             dimnames = list(NULL, extra_cols)) %>%
      tibble::as_tibble() %>%
      bind_cols(out, .)
  }
  return(out)
}

do_aggregate <- function(d, grain, spread, fill, fun, default_fun) {

  start_rows <- nrow(d)
  exp_groups <- d %>%
    count(!!grain, !!spread) %>%
    nrow()
  # Define "safe" version of aggregate_rows for error handling
  ar <- purrr::safely(aggregate_rows)
  d <- ar(d, grain, spread, fill, fun)
  # If aggregate_rows didn't error, return result
  if (nrow(d$result) != exp_groups) {
    stop("No aggregration occured, check your'fun'.")
  } else if (is.null(d$error)) {
    # If the user didn't provide fun, and aggregation happened warn that we'll use sum
    if (default_fun && nrow(d$result) < start_rows) {
      message("There are rows that contain the same values of both ",
              rlang::get_expr(grain), " and ", rlang::get_expr(spread),
              " but you didn't provide a function to 'fun' for their ",
              "aggregation. Proceeding with the default: fun = sum.")
    }
    return(d$result)
    # Otherwise print informative message if aggregation failed
  } else {
    err <- d$error[[1]]
    if (grepl("must be length 1", err)) {
      stop("Aggregation with 'fun' produced more than one value for some",
           " grain-by-spread combinations. Make sure fun is an aggregating",
           " function.")
    } else {
      stop(err)
    }
  }
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
#' @importFrom data.table dcast.data.table
#' @importFrom data.table as.data.table
#' @importFrom tibble as_tibble
#' @return Pivoted tibble. One row for each grain; one column for each spread
#' @noRd
pivot_maker <- function(d, grain, spread, fill, missing_fill) {
  d <- data.table::as.data.table(d)
  f <- stats::formula(paste(rlang::quo_name(grain), "~", rlang::quo_name(spread)))
  d <- data.table::dcast.data.table(data = d,
                                    formula = f,
                                    fill = missing_fill,
                                    value.var = rlang::quo_name(fill))
  d <- tibble::as_tibble(d)
  ## Add spread as prefix to nonID columns
  names(d)[2:ncol(d)] <- paste0(rlang::quo_name(spread), "_", names(d)[2:ncol(d)])
  return(d)
}
