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
