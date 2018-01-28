#' @title Prepare data for machine learning
#'
#' @description \code{prep_data} will prepare your data for use with
#'   \code{\link{tune_models}} or other machine learning packages. Data can be
#'   transformed in the following ways: \enumerate{ \item{Convert columns with
#'   only 0/1 to factor} \item{Remove columns with near-zero variance}
#'   \item{Convert date columns to useful features} \item{Fill in missing values
#'   with various imputation methods} \item{Collapse rare categories into
#'   `other`} \item{Center numeric columns} \item{Standardize numeric columns}
#'   \item{Create dummy variables} } While preparing your data, a recipe will be
#'   generated for identical transformation of future data and stored in the
#'   `rec_obj` attribute of the output data frame. If a recipe object is passed
#'   to `prep_data` via the `rec_obj` argument, that recipe will be applied to
#'   the data. This allows you to transform data in model training and apply
#'   exactly the same transformations in model testing and deployment. The new
#'   data must be identical in structure to the data that the recipe was
#'   prepared with.
#'
#' @param d A dataframe or tibble containing data to impute.
#' @param ... Optional. Unquoted variable names to not be prepped. These will be
#'   returned unaltered. Typically ID and outcome columns would go here.
#' @param rec_obj Optional. A `recipes` object or data frame containing a
#'   `recipes` object in the `rec_obj` attribute slot (as returned from this
#'   function). If present, that recipe will be applied and other arguments will
#'   be ignored.
#' @param outcome Optional. Unquoted column name that indicates the target
#' variable. If this target is 0/1, it will be coerced to Y/N
#' @param remove_near_zero_variance Logical. If TRUE (default), columns with
#'   near-zero variance will be removed. These columns are either a single
#'   value, or meet both of the following criteria: 1. they have very few unique
#'   values relative to the number of samples and 2. the ratio of the frequency
#'   of the most common value to the frequency of the second most common value
#'   is large.
#' @param convert_dates Logical or character. If TRUE (default), day-of-week,
#'   month, and year columns are generated from date columns and date columns
#'   are removed. If FALSE, date columns are removed. If a character vector, it
#'   is passed to the `features` argument of `recipes::step_date`. E.g. if you
#'   want only quarter and year back: `convert_dates = c("quarter", "year")`.
#' @param impute Logical or list. If TRUE (default), columns will be imputed
#'   using mean (numeric), and new category (nominal). If FALSE, data will not
#'   be imputed. If list, possible values are `numeric_method`,
#'   `nominal_method`, `numeric_params`, `nominal_params` and are passed into
#'   the arguments of `hcai_impute`.
#' @param collapse_rare_factors Logical or numeric. If TRUE (default), factor
#'   levels representing less than 3 percent of observations will be collapsed
#'   into a new category, `other`. If numeric, must be in {0, 1} the proportion
#'   of observations below which levels will be grouped into other. See
#'   `recipes::step_other`.
#' @param center Logical. If TRUE, numeric columns will be
#'   centered to have a mean of 0. Default is FALSE.
#' @param scale Logical. If TRUE, numeric columns will be scaled to have a
#'   standard deviation of 1. Default is FALSE.
#' @param dummies Logical. If TRUE, dummy columns will be created. Note most
#'   machine learning algorithms in R are more efficient when dummies are not
#'   provided.
#' @param verbose Logical. If TRUE, verbose console output will describe every
#'   step. This output can be called (even when verbose is FALSE) using the
#'   print method on a prepped data frame.
#'
#' @return Prepared data frame with reusable recipe object for future data
#'   preparation in attribute "rec_obj". Also a summary of how the data was
#'   prepared in attribute "prep_summary".
#' @export
#' @seealso \code{\link{hcai_impute}}
#'
#' @examples
#' d <- pima_diabetes
#'
#' d_train <- d[1:700, ]
#' d_test <- d[701:768, ]
#'
#' # Prep data and recipe. Ignore grain and target columns.
#' d_train_clean <- prep_data(d = d_train,
#'                            patient_id,
#'                            diabetes)
#' saved_recipe <- attr(d_train_clean, "rec_obj")
#'
#' # Apply to new data
#' d_test_clean <- prep_data(d = d_test,
#'                           patient_id,
#'                           diabetes,
#'                           rec_obj = saved_recipe)
#' # Print a summary of what was done
#' print(d_train_clean)
#'
#' # Use non-defaults for prep:
#' d_train_clean <- prep_data(d = d_train,
#'                            patient_id,
#'                            diabetes,
#'                            impute = list(numeric_method = "bagimpute",
#'                                          nominal_method = "bagimpute"),
#'                            collapse_rare_factors = FALSE,
#'                            center = TRUE,
#'                            scale = TRUE,
#'                            dummies = TRUE)
#'
prep_data <- function(d,
                      ...,
                      outcome,
                      rec_obj = NULL,
                      remove_near_zero_variance = TRUE,
                      convert_dates = TRUE,
                      impute = TRUE,
                      collapse_rare_factors = TRUE,
                      center = FALSE,
                      scale = FALSE,
                      dummies = FALSE,
                      verbose = FALSE) {
  # Check to make sure that d is a dframe
  if (!is.data.frame(d)) {
    stop("\"d\" must be a tibble or dataframe.")
  }

  # If outcome is present, validate it.
  d_ignore = NULL
  outcome <- rlang::enquo(outcome)
  if (!rlang::quo_is_missing(outcome)) {
    outcome_name <- rlang::quo_name(outcome)
    if (!(outcome_name %in% names(d))) {
      stop(paste(outcome_name, " not found in d."))
    }
    # Check if there are NAs in the target
    if (any(is.na(dplyr::pull(d, !!outcome)))) {
      stop(paste("Found NA values in the outcome column. Clean your data or",
                 "remove these rows before training a model."))
    }
    # Change target to y/n if it's 0/1
    if (find_0_1_cols(dplyr::select(d, !!outcome)) == outcome_name) {
      d[outcome_name] <- ifelse(dplyr::select(d, !!outcome) == 1, 'y', 'n')
    }
    # Add target to d_ignore and remove from d
    d_ignore <- dplyr::select(d, !!outcome)
    d <- dplyr::select(d, -!!outcome)
  }

  ignore_columns <- rlang::quos(...)
  if (length(ignore_columns)) {
    ignored <- purrr::map_chr(ignore_columns, rlang::quo_name)
    present <- ignored %in% names(d)
    if (any(!present))
      stop(paste(ignored[!present], collapse = ", "), " not found in d.")

    # Separate data into ignored and not
    d_ignore <- dplyr::bind_cols(d_ignore, dplyr::select(d, !!ignored))
    d <- dplyr::select(d, -dplyr::one_of(ignored))

    # Warn if ignored columns have missingness
    m <- missingness(d_ignore) %>%
      dplyr::filter(percent_missing > 0)
    if (!purrr::is_empty(m$variable)) {
      warning("These ignored variables still have missingness: ",
              paste(m$variable, collapse = ", "))
      # TODO: Refactor imputation summary in impute to be also used here. Or just
      # call impute rather than hcai_impute
    }
  }

  # If a recipe or data frame is provided in rec_obj, skip building the recipe
  if (!is.null(rec_obj)) {
    rec_obj <- check_rec_obj(rec_obj)
    if (verbose) {
      message("Using loaded recipe on the new data")
    }
    # ... Else, build recipe step by step
  } else {
    if (verbose) {
      message("Training new recipe")
    }
    # Initialize recipe
    rec_obj <- d %>%
      recipes::recipe(formula = "~.")  # nolint

    # Find largely missing columns and convert to factors
    # rec_obj <- rec_obj %>% step_hcai_mostly_missing_to_factor()  # nolint

    # Remove columns with near zero variance ----------------------------------
    if (remove_near_zero_variance) {
      rec_obj <- rec_obj %>%
        recipes::step_nzv(all_predictors())
    }

    # Convert date columns to useful features and remove original. ------------
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
      if (!purrr::is_empty(cols)) {
        rec_obj <-
          do.call(recipes::step_date,
                  list(recipe = rec_obj, cols, features = sdf)) %>%
          recipes::step_rm(cols)
      }
    } else {
      cols <- find_date_cols(d)
      if (!purrr::is_empty(cols)) {
        rec_obj <- rec_obj %>%
          recipes::step_rm(cols)
      }
      if (verbose) {
        warning("These date columns will be removed: ",
                paste(cols, collapse = ", "))
      }
    }

    # Impute ------------------------------------------------------------------
    if (isTRUE(impute)) {
      rec_obj <- rec_obj %>%
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
      rec_obj <- rec_obj %>%
        hcai_impute(numeric_method = ip$numeric_method,
                    nominal_method = ip$nominal_method,
                    numeric_params = ip$numeric_params,
                    nominal_params = ip$nominal_params)
    } else if (impute != FALSE) {
      stop("impute must be boolean or list.")
    }

    # Collapse rare factors into "other" --------------------------------------
    if (!is.logical(collapse_rare_factors)) {
      if (!is.numeric(collapse_rare_factors))
        stop("collapse_rare_factors must be logical or numeric")
      if (collapse_rare_factors >= 1 || collapse_rare_factors < 0)
        stop("If numeric, collapse_rare_factors should be between 0 and 1.")
      fac_thresh <- collapse_rare_factors
      collapse_rare_factors <- TRUE
    }
    if (collapse_rare_factors) {
      if (!exists("fac_thresh"))
        fac_thresh <- 0.03
      rec_obj <- rec_obj %>%
        recipes::step_other(all_nominal(), threshold = fac_thresh)
    }

    # Log transform
    # Saving until columns can be specified

    # Center ------------------------------------------------------------------
    if (center) {
      rec_obj <- rec_obj %>%
        recipes::step_center(all_numeric())
    }

    # Scale -------------------------------------------------------------------
    if (scale) {
      rec_obj <- rec_obj %>%
        recipes::step_scale(all_numeric())
    }

    # Dummies -----------------------------------------------------------------
    if (dummies) {
      rec_obj <- rec_obj %>%
        recipes::step_dummy(all_nominal())
    }

    # Prep the newly built recipe ---------------------------------------------
    rec_obj <- recipes::prep(rec_obj, training = d)
  }
  # Bake either the newly built or passed-in recipe
  d <- recipes::bake(rec_obj, d)
  # Add ignore columns back in and attach recipe
  d <- dplyr::bind_cols(d_ignore, d)
  attr(d, "rec_obj") <- rec_obj

  # TODO: Remove unused factor levels with droplevels.

  # Build up prep_data summary for verbose output.
  # junk <- utils::capture.output(prep_summary$steps <- print(rec_obj))
  # prep_summary$baked_data <- summary(rec_obj)

  # if (!is.null(d_ignore)) {
  #   ignored_types <- purrr::map(d_ignore, function(x) {
  #     if (is.numeric(x)) {
  #       return("numeric")
  #     } else if (is.factor(x) || is.character(x)) {
  #       return("nominal")
  #     }
  #   })
  #   prep_summary$baked_data <-
  #     dplyr::bind_rows(tibble::tibble(variable = ignored,
  #                                     type = as.character(ignored_types),
  #                                     role = "ignored",
  #                                     source = "original"),
  #                      prep_summary$baked_data)
  # }

  # attach prep_summary to data
  # attr(d, "prep_summary") <- prep_summary

  # Add class signature to data frame so we can ID for imputation in deployment
  class(d) <- c("hcai_prepped_df", class(d))

  if (verbose) {
    print(d)
  }

  return(d)
}

# print method for prep_data
#' @export
print.hcai_prepped_df <- function(x, ...) {
  s <- attr(x, "prep_summary")
  message("Original missingness and methods used in data prep:")
  print(s$missingness)
  message("These steps were performed on the data:")
  print(s$steps)
  message("Baked data column types:")
  print(s$baked_data)
  message("Preview of baked data:")
  print.data.frame(x[1:5, ])
  return(invisible(x))
}
