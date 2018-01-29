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
#'   variable. If provided, argument must be named. If this target is 0/1, it
#'   will be coerced to Y/N
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
#' @param center Logical. If TRUE, numeric columns will be centered to have a
#'   mean of 0. Default is FALSE.
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
                      factor_outcome = TRUE,
                      verbose = FALSE) {
  # Check to make sure that d is a dframe
  if (!is.data.frame(d)) {
    stop("\"d\" must be a tibble or dataframe.")
  }
  # Deal with "..." columns to be ignored
  ignore_columns <- rlang::quos(...)
  ignored <- purrr::map_chr(ignore_columns, rlang::quo_name)
  d_ignore <- NULL
  if (length(ignore_columns)) {
    present <- ignored %in% names(d)
    if (any(!present))
      stop(paste(ignored[!present], collapse = ", "), " not found in d.")
    # Separate data into ignored and not
    d_ignore <- dplyr::select(d, !!ignored)
    d <- dplyr::select(d, -dplyr::one_of(ignored))
    # Warn if ignored columns have missingness
    m <- missingness(d_ignore) %>%
      dplyr::filter(percent_missing > 0)
    if (!purrr::is_empty(m$variable))
      warning("These ignored variables have missingness: ",
              paste(m$variable, collapse = ", "))
  }

  # If there's a recipe in rec_obj, use that
  if (!is.null(rec_obj)) {
    if (verbose) message("Using loaded recipe on the new data")
    rec_obj <- check_rec_obj(rec_obj)
    # Look for variables that weren't present in training, add them to ignored
    newvars <- setdiff(names(d), c(rec_obj$var_info$variable,
                                   attr(rec_obj, "ignored_columns")))
    if (length(newvars)) {
      warning("These variables were not observed in training ",
              "and will be ignored: ", paste(newvars, collapse = ", "))
      ignored <- c(ignored, newvars)
      d_ignore <- dplyr::bind_cols(d_ignore, dplyr::select(d, !!newvars))
    }
    # Look for variables unignored in traiing but missing here and error
    missing_vars <- setdiff(rec_obj$var_info$variable, names(d))
    if (length(missing_vars))
      stop("These variables were present in training but are missing here: ",
           paste(newvars, collapse = ", "))
  } else {

    # Initialize a new recipe
    if (verbose) message("Training new recipe")
    ## Start by making all variables predictors...
    rec_obj <- recipes::recipe(d, ~ .)
    ## Then deal with outcome if present
    outcome <- rlang::enquo(outcome)
    if (!rlang::quo_is_missing(outcome)) {
      outcome_name <- rlang::quo_name(outcome)
      if (!outcome_name %in% names(d))
        stop(paste(outcome_name, " not found in d."))
      # Check if there are NAs in the target
      outcome_vec <- dplyr::pull(d, !!outcome)
      if (any(is.na(outcome_vec)))
        stop("Found NA values in the outcome column. Clean your data or ",
             "remove these rows before training a model.")
      # Changing the role of outcome from predictor to outcome warns, shush:
      suppressWarnings({
        rec_obj <- recipes::add_role(rec_obj, !!outcome, new_role = "outcome")
      })

      # If outcome is binary 0/1, convert to N/Y -----------------------------
      if (factor_outcome && all(outcome_vec %in% 0:1)) {
        rec_obj <- rec_obj %>%
          recipes::step_bin2factor(all_outcomes(), levels = c("Y", "N"))
      }
    }

    # Build recipe step-by-step:

    # TODO
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
        rec_obj <- recipes::step_rm(rec_obj, cols)
      }
      if (verbose)
        warning("These date columns will be removed: ",
                paste(cols, collapse = ", "))
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
  # Add ignore columns back in and attach as attribute to recipe
  d <- dplyr::bind_cols(d_ignore, d)
  attr(rec_obj, "ignored_columns") <- unname(ignored)
  attr(d, "rec_obj") <- rec_obj
  class(d) <- c("hcai_prepped_df", class(d))

  # TODO: Remove unused factor levels with droplevels. (MAYBE)
  return(d)
}

# print method for prep_data
#' @export
print.hcai_prepped_df <- function(x, ...) {
  message("healthcareai prepped data frame. Recipe used to prepare data:\n")
  print(attr(x, "rec_obj"))
  NextMethod(x)
  return(invisible(x))
}
