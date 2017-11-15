#' pivot
#'
#' @param d 
#' @param grain 
#' @param spread 
#' @param fill Column to be used to fill the values of cells in the output, 
#' after aggregation if \code{fun} is provided. If \code{fill} is not provided,
#' counts will be used (as though a fill column of all 1s had been provided.)
#' @param fun Bare (unquoted) function to handle aggreation
#'
#' @return
#' @export
#' @importFrom rlang :=
#'
#' @examples
pivot <- function(d, grain, spread, fill, fun = sum) {
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
    fill <- quo(fill_ones)
    d <- mutate(d, !!quo_name(fill) := rep(1L, nrow(d)))
  }
  
  # Make sure there's no missingness in grain or spread
  missing_check(d, grain)
  missing_check(d, spread)
  
  # Make sure fun is a function
  if (!is.function(fun)) stop(match.call()$fun, "isn't a function")
  
  # Convert grouping variables to factors
  d <- 
    d %>%
    dplyr::mutate(!!quo_name(grain) := as.factor(!!grain),
                  !!quo_name(spread) := as.factor(!!spread))
  # Pull factor levels to use as row deliniators and column names
  to_rows <- levels(pull(d, !!grain))
  to_cols <- levels(pull(d, !!spread))

  # Check if there are any grain-spread pairs that have more than one entry...
  need_aggregate <- any(duplicated(dplyr::select(d, !!grain, !!spread)))
  # ... If there are, aggregate rows
  if (need_aggregate) 
    d <- aggregate_rows(d, grain, spread, fill, fun)
  
  # Create a data frame full of zeros, which will be replaced where appropriate
  # Critically, columns are arranged by the integer representation of spread
  # which allows us to index the entries by those integers
  out <- data.frame(matrix(0, nrow = length(to_rows), ncol = length(to_cols),
                           dimnames = list(NULL, to_cols))) 
  out <- mutate(out, !!quo_name(grain) := to_rows)
  # Loop over each row.
  out <- 
    purrr::map_df(to_rows,  ~ {
      # Pull the current row of interest
      keep <- filter(out, (!!grain) == .x)
      # Inside-out:
      # i IDs rows in original dataframe that we want in this row of output
      # Subsetting `d[[spread]]` by those rows gives us factor levels to fill in
      # Converting them to integers gives the indices to fill in
      i <- which(pull(d, !!grain) == .x)
      keep[as.integer(pull(d, !!spread)[i])] <- pull(d, !!fill)[i]
      return(keep)
    })  

  # Arrange and rename columns
  ## Put the grain column first
  out <- out[, c(ncol(out), 1:(ncol(out) - 1))]
  ## Add spread as prefix to non-ID columns
  names(out)[2:ncol(out)] <- 
    paste0(rlang::quo_name(spread), "_", names(out)[2:ncol(out)])
  
  return(out)
}

#' Check column in data frame for any missingness
#'
#' @param d data frame
#' @param quo_var a quosure'd variable, probably captured by rlang::enquo
#' in the parent function
#'
#' @return TRUE if no missingness. Otherwise stops with an informative message.
#' @noRd
missing_check <- function(d, quo_var) {
  if (!any(is.na(pull(d, !!quo_var)))) {
    return(TRUE)
  } else {
    called_from <- sys.call(1)[[1]]
    stop("Fill in missingness in ", rlang::get_expr(quo_var),
         " before calling ", called_from)
  }
}

aggregate_rows <- function(d, grain, spread, fill, fun) {
  
  # Test if the user provided a 'fun'. If not warn that we'll use sum
  if (is.null(match.call(call = sys.call(1))$fun)) 
    warning("There are some duplicate pairs of ", rlang::get_expr(grain), 
            " and ", rlang::get_expr(spread), " and you didn't provide an ",
            "aggregating function to \"fun\". Using the default function sum ",
            "to aggregate.")
  # Aggregate
  d <- 
    d %>%
    group_by(!!grain, !!spread) %>%
    summarize(!!quo_name(fill) := fun(!!fill))
  
  return(d)
}


set.seed(405)
dd <- data.frame(
  identity = sample(letters[1:4], 5, TRUE),
  category = sample(LETTERS, 5, TRUE),
  ff = sample(100, 5)
)
dd
pivot(dd, identity, category, ff)
# dd$identity[3] <- NA
# pivot(dd, identity, category, fun = mean)
# pivot(dd, "identity", "category")
# pivot(dd, "identity", "category", "ff")
# dd$category[4] <- "H"
# pivot(dd, "identity", "category", "ff", sum)

