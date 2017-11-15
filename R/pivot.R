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
#' 
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @examples
dummify <- function(d, grain, spread, fill, fun = sum) {
  # dummify "spreads" spread into separate columns, creating one row for 
  # each entry in grain
  # fill can be the name of a column in d containing value to fill in
  # otherwise it should be a single value that value will be used to fill in 
  # all present values
  # Other columns in d will be dropped
  
  require(purrr); require(dplyr)
  
  # Capture argument column names as quosures
  grain <- rlang::enquo(grain)
  spread <- rlang::enquo(spread)
  fill <- rlang::enquo(fill)
  
  # If fill wasn't provided, create and use a column with all 1s, with message
  if (rlang::quo_is_missing(fill)) {
    message("No fill column was provided, so using \"1\" for present entities")
    fill <- quo(fill_ones)
    d <- mutate(d, !!quo_name(fill) := rep(1L, nrow(d)))
  }
  
  # Check inputs
  # Make sure fun is a function
  if (!is.function(fun)) stop(match.call()$fun, "isn't a function")
  
  # Convert grouping variables to factors
  d <- 
    d %>%
    dplyr::mutate(!!quo_name(grain) := as.factor(!!grain),
                  !!quo_name(spread) := as.factor(!!spread))
  
  # Check if there are any grain-spread pairs that have more than one entry.
  
  need_aggregate <- any(duplicated(dplyr::select(d, !!grain, !!spread)))
  if (need_aggregate) 
    d <- aggregate_rows(d, grain, spread, fill, fun)

  browser()
  stop()
  
  
  
  
  if (any(is.na(d[[spread]])))
    stop("There's missingness in ", spread, ". Deal with it before dummifying.")
  
  # Identify what to turn into rows and columns
  toRows <- levels(d[[grain]])
  toCols <- levels(d[[spread]])
  
  # If fill isn't a column in d, repeat it; otherwise pull it out of d
  fill <- if (!fill %in% names(d)) rep(fill, nrow(d)) else d[[fill]]
  
  # Create a data frame full of zeros, which will be replaced where appropriate
  # Critically, columns are arranged by the integer representation of spread
  # which allows us to index the entries by those integers
  out <- data.frame(matrix(0, nrow = length(toRows), ncol = length(toCols),
                           dimnames = list(NULL, toCols))) 
  out$id <- toRows
  # Loop over each row. Maybe this should be apply(out, 1, ...). Test speed.
  out <- 
    map_df(toRows,  ~ {
      # To do: Fill with 1 by default, or take additional column with fill values
      # Pull the current row of interest
      keep <- filter(out, id == .x)
      # Inside-out:
      # i IDs rows in original dataframe that we want in this row of output
      # Subsetting `d[[spread]]` by those rows gives us factor levels to fill in
      # Converting them to integers gives the indices to fill in
      i <- which(d[[grain]] == .x)
      keep[as.integer(d[[spread]][i])] <- fill[i]
      return(keep)
    })  
  
  # Arrange and rename columns
  out <- out[, c(ncol(out), 1:(ncol(out) - 1))]
  names(out)[1] <- grain
  names(out)[2:ncol(out)] <- paste0(spread, "_", names(out)[2:ncol(out)])
  
  return(out)
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

dummify(dd, identity, category, fun = mean)

# set.seed(405)
# dd <- data.frame(
#   identity = sample(letters[1:4], 5, TRUE),
#   category = sample(LETTERS, 5, TRUE),
#   ff = sample(100, 5)
# )
# dd
# dummify(dd, "identity", "category")
# dummify(dd, "identity", "category", "ff")
# dd$category[4] <- "H"
# dummify(dd, "identity", "category", "ff", sum)
