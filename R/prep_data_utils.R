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
  if (class(attr(recipe, "recipe")) == "recipe")
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

#' @title Convert character date columns to dates
#'
#' @description I try to convert columns ending in "DTS" to type Date. I'll take
#' my best guess at the format and return a more standard one if I can. Times
#' will be removed.
#'
#' @param d A dataframe or tibble containing data to try to convert to dates.
#'
#' @return A tibble containing the converted date columns. If no columns
#' needed conversion, the original data will be returned.
#' @import purrr
#' @importFrom lubridate guess_formats date
#' @export
#'
#' @examples
#' d <- tibble::tibble(a_DTS = c("2018-3-25", "2018-3-25"),
#' b_nums = c(2, 4),
#' c_DTS = c("03-25-2018", "03-25-2018"),
#' d_chars = c("a", "b"),
#' e_date = lubridate::mdy(c("3-25-2018", "3-25-2018")))
#'
#' d <- convert_date_cols(d)
#'
convert_date_cols <- function(d) {

  # Extract date columns only
  dd <- d[1, find_date_cols(d)]

  date_formats <- map(dd,
                      guess_formats,
                      orders = c("ymd", "mdy", "ymd HMS", "mdy HMS"))

  # Find working formats
  valid_formats <- map2(dd, date_formats, function(x, y) {
    temp <- map2(x, y, function(x, y) {
      as.POSIXct(x = x, format = y)
    })
    # Can't use is.POSIXct here because NA has class POSIXct. wtf.
    unlist(map(temp, function(x) {
      !is.na(x)
    }))
  })

  # Collapse to one format and find columns with no valid format
  valid_formats <- map_int(valid_formats, function(x) {
    x <- match(TRUE, x)
  })
  bad_date_cols <- names(valid_formats[is.na(valid_formats)])

  if (length(bad_date_cols)) {
    stop(paste("Unable to convert the following columns to dates. Convert",
         "them to ymd, mdy, ymd hms, or mdy hms format. See lubridate::as_date",
         "or as.POSIXct for more info. \n"), paste(bad_date_cols, collapse = ", "))
  }

  # Remove formats that don't work
  dd <- dd %>% select(-one_of(bad_date_cols))
  date_formats <- date_formats[!is.na(valid_formats)]
  valid_formats <- valid_formats[!is.na(valid_formats)]

  # Working formats
  use_formats <- map2_chr(date_formats, valid_formats, `[[`)

  # Convert dates
  dd <- d[, find_date_cols(d)]
  dd <- map2_df(dd, use_formats, function(x, y) {
    date(as.POSIXct(x = x, format = y))
  })

  # Replace and reorder original dataframe
  d[names(dd)] <- dd

  return(tibble::as_tibble(d))
}
