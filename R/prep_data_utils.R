#' Returns names of columns that are entirely 0/1 and numeric.
#' @noRd
find_0_1_cols <- function(d) {
  # Returns names of columns that have only 0/1 in them.
  cols <- purrr::map_lgl(d, ~ all(.x %in% c(0L, 1L, NA_integer_)))
  if (any(cols == TRUE)) {
    return(names(d)[cols])
  } else {
    return(FALSE)
  }
}

#' Returns names of columns that are more than 80% missing by default.
#' Missing percentage threshold to return can be adjusted.
#' @noRd
find_mostly_missing_cols <- function(d, percent_missing_threshold = 80) {
  # Finds columns with more missing values than the threshold
  cols <-
    missingness(d) %>%
    dplyr::filter(percent_missing > percent_missing_threshold)
  return(cols$variable)
}

#' Returns names of date columns. Looks for both date classes and "DTS" in the
#' column names.
#' @importFrom lubridate is.Date
#' @importFrom lubridate is.POSIXt
#' @noRd
find_date_cols <- function(d) {
  by_class <-
    purrr::map_lgl(d, ~ lubridate::is.Date(.x) || lubridate::is.POSIXt(.x))
  by_name <- purrr::map_lgl(names(d), ~grepl("DTS$", .x))
  return(names(d)[by_class | by_name])
}

#' If recipe was provided to function calling check_rec_obj, check that it's
#' not a missing attribute and that it's either a recipes object or a data
#' frame containing a recipes object in the "recipe" attr. If the latter,
#' return the recipe.
#' @noRd
check_rec_obj <- function(recipe) {
  rec_obj_provided <- sys.call(sys.parent(1))$recipe
  if (is.null(recipe) && !is.null(rec_obj_provided))
    stop("Attribute \"", rec_obj_provided[[3]], "\" not found in ",
         rec_obj_provided[[2]])
  # If there is a recipe object in the attribute slot, use it
  if (is(attr(recipe, "recipe"))[1] == "recipe")
    recipe <- attr(recipe, "recipe")
  # Check to make sure recipe is a valid recipe
  if (!inherits(recipe, "recipe") && !is.null(recipe)) {
    stop("\"recipe\" must be a valid recipe object or a dataframe with ",
         "a valid recipes object in the recipe attribute.")
  }
  return(recipe)
}

#' Find predictors with missingness that didn't have missingness in recipe
#' training
#' @noRd
find_new_missingness <- function(d, recipe) {
  predictors <- recipe[["var_info"]]$variable[recipe[["var_info"]]$role == "predictor"]
  missing_then <- attr(recipe, "missingness") %>% .[. > 0] %>% names()
  missing_now <- missingness(d, return_df = FALSE) %>% .[. > 0] %>% names()
  new_missing <- dplyr::setdiff(missing_now, missing_then)
  return(dplyr::intersect(new_missing, predictors))
}

#' @title Convert character date columns to dates and times
#'
#' @description This function is called in \code{\link{prep_data}} and so it
#'   shouldn't usually need to be called directly. It tries to convert columns
#'   ending in "DTS" to type Date or DateTime (POSIXt). It makes a best guess at
#'   the format and return a more standard one if possible.
#'
#' @param d A dataframe or tibble containing data to try to convert to dates.
#'
#' @return A tibble containing the converted date columns. If no columns needed
#'   conversion, the original data will be returned.
#' @import purrr
#' @importFrom lubridate guess_formats date
#' @export
#'
#' @examples
#' d <- tibble::tibble(a_DTS = c("2018-3-25", "2018-3-25"),
#'                     b_nums = c(2, 4),
#'                     c_DTS = c("03-01-2018", "03-07-2018"),
#'                     d_chars = c("a", "b"),
#'                     e_date = lubridate::mdy(c("3-25-2018", "3-25-2018")))
#' convert_date_cols(d)
convert_date_cols <- function(d) {
  # Extract character date columns only
  col_names <- names(d)[map_lgl(names(d), ~ grepl("DTS$", .x))]
  dates <- map_lgl(d[, col_names], ~ lubridate::is.Date(.x) || lubridate::is.POSIXt(.x))
  col_names <- col_names[!dates]
  dd <- tibble::as_tibble(d[1, col_names, drop = FALSE])

  # Nothing to convert
  if (!length(dd))
    return(d)

  # Guess formats for each column
  date_formats <- map(dd,
                      guess_formats,
                      orders = c("ymd", "mdy", "ymd HMS", "mdy HMS"))

  # Build tibble with name, first entry in date columns, and guessed formats
  d_dates <- tibble(names = col_names,
                    first_entry = as.character(
                      slice(dd, 1) %>% unlist(., use.names = FALSE)),
                    guessed_formats = date_formats,
                    has_guesses = !(map_lgl(guessed_formats, is.null)))

  # Remove rows with no guessed formats, save the names, and unnest
  bad_date_cols <- d_dates %>%
    filter(has_guesses == FALSE) %>%
    pull(names)

  d_dates <- d_dates %>%
    filter(has_guesses == TRUE) %>%
    tidyr::unnest(cols = guessed_formats)

  # Try to convert each row using the format, then group by name
  d_dates <- d_dates %>%
    mutate(converted_date = as.POSIXct(
      x = first_entry, format = guessed_formats),
      is_valid = !(is.na(converted_date))) %>%
    group_by(names)

  # Remove names that have only non-working formats
  more_bad <- d_dates %>%
    summarize(any_valid = any(is_valid)) %>%
    filter(any_valid == FALSE) %>%
    pull(names)

  # Error for no guessed formats or no working guessed formats
  bad_date_cols <- c(bad_date_cols, more_bad)
  if (length(bad_date_cols)) {
    stop(paste("Unable to convert the following columns to dates. Convert",
         "them to ymd, mdy, ymd hms, or mdy hms format. See lubridate::as_date",
         "or as.POSIXct for more info. \n"), list_variables(bad_date_cols))
  }

  # Collect working formats
  d_dates <- d_dates %>%
    summarize(converted_date = last(converted_date),
              working_format = last(guessed_formats)) %>%
    arrange(names)

  # Ensure format order and column order is the same.
  dd <- d[, col_names[order(col_names)]]

  # Convert dates in original data
  stopifnot(all.equal(names(dd), d_dates$names))
  dd <- map2_df(dd, d_dates$working_format, function(x, y) {
    if (grepl("H", y))
      out <- as.POSIXct(x = x, format = y)
    else
      out <- date(as.POSIXct(x = x, format = y))
  })

  # Replace and reorder original dataframe
  d[names(dd)] <- dd

  return(tibble::as_tibble(d))
}
