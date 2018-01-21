#' Title
#'
#' @param d
#' @param ...
#' @param rec_obj
#' @param convert_0_1_to_factor
#' @param convert_dates Logical or character. If TRUE (default), day-of-week,
#'   month, and year columns are generated from date columns and date columns
#'   are removed. If FALSE, date columns are left intact. If a character vector,
#'   it is passed to the `features` argument of `recipes::step_date`. E.g. if
#'   you want only month and year back: `convert_dates = c("quarter", "year")`.
#' @param collapse_rare_factors
#' @param remove_near_zero_variance
#' @param impute
#' @param center
#' @param scale
#' @param dummies
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
prep_data <- function(d = NULL,
                      ...,
                      rec_obj = NULL,
                      convert_0_1_to_factor = TRUE,
                      convert_dates = TRUE,
                      collapse_rare_factors = TRUE,
                      remove_near_zero_variance = TRUE,
                      impute = TRUE,
                      center = FALSE,
                      scale = FALSE,
                      dummies = FALSE,
                      verbose = FALSE) {
  # Check to make sure that d is a dframe
  if (!is.data.frame(d)) {
    stop("\"d\" must be a tibble or dataframe.")
  }
  ignore_columns <- rlang::quos(...)
  if (length(ignore_columns)) {
    ignored <- purrr::map_chr(ignore_columns, rlang::quo_name)
    present <- ignored %in% names(d)
    if (any(!present))
      stop(paste(ignored[!present], collapse = ", "), " not found in d.")

    # Separate data into ignored and not
    d_ignore <- dplyr::select(d, !!!ignored)
    d <- dplyr::select(d, -dplyr::one_of(ignored))
  } else {
    d_ignore <- NULL
  }

  ##### TO DO
  # if (!is.null(rec_obj)) -- pull check and grab-from-attr from impute.R
  #   Apply recipe and return
  #####

  # Initialize recipe
  rec <- d %>%
    recipe(formula = "~.")

  # Find largely missing columns and convert to factors
  # rec <- rec %>% step_hcai_mostly_missing_to_factor()

  # Convert 0/1 columns to factors (step_bin2factor)
  if (convert_0_1_to_factor) {
    cols <- find_0_1_cols(d)
    rec <- rec %>%
      step_bin2factor(!!cols, levels = c("Y", "N"))
  }

  # Convert date columns to useful features and remove original.
  if (!is.logical(convert_dates)) {
    if (!is.character(convert_dates))  #  || is.null(names(convert_date))
      stop("convert_dates must be logical or features for step_date")
    sdf <- convert_dates
    convert_dates <- TRUE
  }
  if (convert_dates) {
    # If user didn't provide features, set them to defaults
    if (!exists("sdf"))
      sdf <- c("dow", "month", "year")
    cols <- find_date_cols(d)
    rec <-
      do.call(step_date, list(recipe = rec, cols, features = sdf)) %>%
      step_rm(cols)
  }

  # Impute
  if (impute) {
    rec <- rec %>%
      hcai_impute()
  } else if (is.list(impute)) {
    # Set defaults
    ip <- list(numeric_method = "mean",
               nominal_method = "new_category",
               numeric_params = NULL,
               nominal_params = NULL)
    ip[names(ip) %in% names(impute)] <- impute[names(impute) %in% names(ip)]
    extras  <- names(impute)[!(names(impute) %in% names(ip))]
    if (length(extras > 0)) {
      warning("You have extra imputation parameters that won't be used: ",
              paste(extras, collapse = ", "),
              ". Available params are: ", paste(names(ip), collapse = ", "))
    }
    # Impute takes defaults or user specified inputs. Error handling inside.
    rec <- rec %>%
      hcai_impute(numeric_method = ip$numeric_method,
                  nominal_method = ip$nominal_method,
                  numeric_params = ip$numeric_params,
                  nominal_params = ip$nominal_params)
  } else if (impute != FALSE) {
    stop("impute must be boolean or list.")
  }

  # Collapse rare factors into "other"
  if (collapse_rare_factors) {
    rec <- rec %>%
      step_other(all_nominal(), threshold = .02)
  }

  # Log transform
  # Saving until columns can be specified

  # Center
  if (center) {
    rec <- rec %>%
      step_center(all_numeric())
  }

  # Scale
  if (scale) {
    rec <- rec %>%
      step_scale(all_numeric())
  }

  # Remove columns with near zero variance
  if (remove_near_zero_variance) {
    rec <- rec %>%
      step_nzv(everything())
  }

  # Dummies
  if (isTRUE(dummies)) {
    rec <- rec %>%
      step_dummy(all_nominal())
  }

  # Apply recipe
  d_clean <- rec %>%
    prep(training = d) %>%
    bake(newdata = d)

  # Add ignore columns back in.
  d_clean <- dplyr::bind_cols(d_ignore, d_clean)

  attr(d_clean, "rec_obj") <- rec
  return(d_clean)
}
