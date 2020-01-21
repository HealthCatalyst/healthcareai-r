#' @title Prepare data for machine learning
#'
#' @description \code{prep_data} will prepare your data for machine learning.
#'   Some steps enhance predictive power, some make sure that the data format is
#'   compatible with a wide array of machine learning algorithms, and others
#'   provide protection against common problems in model deployment. The
#'   following steps are available; those followed by * are applied by default.
#'   Many have customization options. \enumerate{ \item{Convert columns with
#'   only 0/1 to factor*} \item{Remove columns with near-zero variance*}
#'   \item{Convert date columns to useful features*} \item{Fill in missing
#'   values via imputation*} \item{Collapse rare categories into "other"*}
#'   \item{Center numeric columns} \item{Standardize numeric columns}
#'   \item{Create dummy variables from categorical variables*} \item{Add
#'   protective levels to factors for rare and missing data*} \item{Convert
#'   columns to principle components using PCA}} While preparing your data, a
#'   recipe will be generated for identical transformation of future data and
#'   stored in the `recipe` attribute of the output data frame.If a recipe
#'   object is passed to `prep_data` via the `recipe` argument, thatrecipe will
#'   be applied to the data. This allows you to transform data inmodel training
#'   and apply exactly the same transformations in model testing and deployment.
#'   The new data must be identical in structure to the data that the recipe was
#'   prepared with.
#'
#' @param d A data frame
#' @param ... Optional. Columns to be ignored in preparation and model training,
#'   e.g. ID columns. Unquoted; any number of columns can be included here.
#' @param outcome Optional. Unquoted column name that indicates the target
#'   variable. If provided, argument must be named. If this target is 0/1, it
#'   will be coerced to Y/N if factor_outcome is TRUE; other manipulation steps
#'   will not be applied to the outcome.
#' @param recipe Optional. Recipe for how to prep d. In model deployment, pass
#'   the output from this function in training to this argument in deployment to
#'   prepare the deployment data identically to how the training data was
#'   prepared. If training data is big, pull the recipe from the "recipe"
#'   attribute of the prepped training data frame and pass that to this
#'   argument. If present, all following arguments will be ignored.
#' @param remove_near_zero_variance Logical or numeric. If TRUE (default),
#'   columns with near-zero variance will be removed. These columns are either a
#'   single value, or the most common value is much more frequent than the
#'   second most common value. Example: In a column with 120 "Male" and 2
#'   "Female", the frequency ratio is 0.0167. It would be excluded by default or
#'   if `remove_near_zero_variance` > 0.0166. Larger values will remove more
#'   columns and this value must lie between 0 and 1.
#' @param convert_dates Logical or character. If TRUE (default), date and time
#'   columns are transformed to circular representation for hour, day, month,
#'   and year for machine learning optimization. If FALSE, date and time columns
#'   are removed. If character, use "continuous" (same as TRUE), "categories",
#'   or "none" (same as FALSE). "categories" makes hour, day, month, and year
#'   readable for interpretation. If \code{make_dummies} is TRUE, each unique
#'   value in these features will become a new dummy variable. This will create
#'   wide data, which is more challenging for some machine learning models. All
#'   features with the DTS suffix will be treated as a date.
#' @param impute Logical or list. If TRUE (default), columns will be imputed
#'   using mean (numeric), and new category (nominal). If FALSE, data will not
#'   be imputed. If this is a list, it must be named, with possible entries for
#'   `numeric_method`, `nominal_method`, `numeric_params`, `nominal_params`,
#'   which are passed to \code{\link{hcai_impute}}.
#' @param collapse_rare_factors Logical or numeric. If TRUE (default), factor
#'   levels representing less than 3 percent of observations will be collapsed
#'   into a new category, `other`. If numeric, must be in {0, 1}, and is the
#'   proportion of observations below which levels will be grouped into other.
#'   See `recipes::step_other`.
#' @param PCA Integer or Logical. PCA reduces training time, particularly for
#'   wide datasets, though it renders models less interpretable." If integer,
#'   represents the number of principal components to convert the numeric data
#'   into. If TRUE, will convert numeric data into 5 principal components. PCA
#'   requires that data is centered and scaled and will set those params to
#'   TRUE. Default is FALSE.
#' @param center Logical. If TRUE, numeric columns will be centered to have a
#'   mean of 0. Default is FALSE, unless PCA is performed, in which case it is
#'   TRUE.
#' @param scale Logical. If TRUE, numeric columns will be scaled to have a
#'   standard deviation of 1. Default is FALSE, unless PCA is performed, in
#'   which case it is TRUE.
#' @param make_dummies Logical or list. If TRUE (default), dummy columns will be
#'   created for categorical variables. When dummy columns are created, columns
#'   are not created for reference levels. By default, the levels are reassigned
#'   so the mode value is the reference level. If a named list is provided,
#'   those values will replace the reference levels. See the example for details.
#' @param add_levels Logical. If TRUE (default), "other" and "missing" will be
#'   added to all nominal columns. This is protective in deployment: new levels
#'   found in deployment will become "other" and missingness in deployment can
#'   become "missing" if the nominal imputation method is "new_category". If
#'   FALSE, these "other" will be added to all nominal variables if
#'   \code{collapse_rare_factors} is used, and "missingness" may be added
#'   depending on details of imputation.
#' @param logical_to_numeric Logical. If TRUE (default), logical variables will
#'   be converted to 0/1 integer variables.
#' @param factor_outcome Logical. If TRUE (default) and if all entries in
#'   outcome are 0 or 1 they will be converted to factor with levels N and Y for
#'   classification. Note that which level is the positive class is set in
#'   training functions rather than here.
#' @param no_prep Logical. If TRUE, overrides all other arguments to FALSE so
#'   that d is returned unmodified, except that character variables may be
#'   coverted to factors and a tibble will be returned even if the input was
#'   a non-tibble data frame.
#'
#' @return Prepared data frame with reusable recipe object for future data
#'   preparation in attribute "recipe". Attribute recipe contains the names of
#'   ignored columns (those passed to ...) in attribute "ignored_columns".
#' @export
#' @seealso To let data preparation happen automatically under the hood, see
#'   \code{\link{machine_learn}}
#'
#'   To take finer control of imputation, see \code{\link{impute}}, and for
#'   finer control of data prep in general check out the recipes package:
#'   \url{https://topepo.github.io/recipes/}
#'
#'   To train models on prepared data, see \code{\link{tune_models}} and
#'   \code{\link{flash_models}}
#'
#' @examples
#' d_train <- pima_diabetes[1:700, ]
#' d_test <- pima_diabetes[701:768, ]
#'
#' # Prep data. Ignore patient_id (identifier) and treat diabetes as outcome
#' d_train_prepped <- prep_data(d = d_train, patient_id, outcome = diabetes)
#'
#' # Prep test data by reapplying the same transformations as to training data
#' d_test_prepped <- prep_data(d_test, recipe = d_train_prepped)
#'
#' # View the transformations applied and the prepped data
#' d_test_prepped
#'
#' # Customize preparations:
#' prep_data(d = d_train, patient_id, outcome = diabetes,
#'           impute = list(numeric_method = "bagimpute",
#'                         nominal_method = "bagimpute"),
#'           collapse_rare_factors = FALSE, center = TRUE, scale = TRUE,
#'           make_dummies = FALSE, remove_near_zero_variance = .02)
#'
#' # Picking reference levels:
#' # Dummy variables are not created for reference levels. Mode levels are
#' # chosen as reference levels by default. The list given to `make_dummies`
#' # sets the reference level for `weight_class` to "normal". All other values
#' # in `weight_class` will create a new dummy column that is relative to normal.
#' prep_data(d = d_train, patient_id, outcome = diabetes,
#'           make_dummies = list(weight_class = "normal"))
#'
#' # `prep_data` also handles date and time features by default:
#' d <-
#'   pima_diabetes %>%
#'   cbind(
#'     admitted_DTS = seq(as.POSIXct("2005-1-1 0:00"),
#'                        length.out = nrow(pima_diabetes), by = "hour")
#'   )
#' d_train = d[1:700, ]
#' prep_data(d = d_train)
#'
#' # Customize how date and time features are handled:
#' # When `convert_dates` is set to "categories", the prepped data will be more
#' # readable, but will be wider.
#' prep_data(d = d_train, convert_dates = "categories")
#'
#' # PCA to reduce training time:
#' \dontrun{
#' start_time <- Sys.time()
#' pd <- prep_data(pima_diabetes, patient_id, outcome = diabetes, PCA = FALSE)
#' ncol(pd)
#' m <- machine_learn(pd, patient_id, outcome = diabetes)
#' end_time <- Sys.time()
#' end_time - start_time
#'
#' start_time <- Sys.time()
#' pcapd <- prep_data(pima_diabetes, patient_id, outcome = diabetes, PCA = TRUE)
#' ncol(pcapd)
#' m <- machine_learn(pcapd, patient_id, outcome = diabetes)
#' Sys.time() - start_time
#' }
prep_data <- function(d,
                      ...,
                      outcome,
                      recipe = NULL,
                      remove_near_zero_variance = TRUE,
                      convert_dates = TRUE,
                      impute = TRUE,
                      collapse_rare_factors = TRUE,
                      PCA = FALSE,
                      center = FALSE,
                      scale = FALSE,
                      make_dummies = TRUE,
                      add_levels = TRUE,
                      logical_to_numeric = TRUE,
                      factor_outcome = TRUE,
                      no_prep = FALSE) {
  # Check to make sure that d is a dframe
  if (!is.data.frame(d))
    stop("\"d\" must be a data frame.")

  orig_data <- d

  new_recipe <- TRUE
  if (!is.null(recipe)) {
    new_recipe <- FALSE
    recipe <- check_rec_obj(recipe)
    no_prep <- attr(recipe, "no_prep")
  }
  if (no_prep)
    remove_near_zero_variance <- convert_dates <- impute <-
      collapse_rare_factors <- center <- scale <- make_dummies <-
      add_levels <- logical_to_numeric <- factor_outcome <- FALSE

  # Capture pre-modification missingness
  d_missing <- missingness(d, return_df = FALSE)
  # Capture original data structure
  d_ods <- d[0, ]
  # Capture factor levels
  d_levels <- get_factor_levels(d)
  # Capture best_levels attributes
  best_levels <- attr(d, "best_levels")

  outcome <- rlang::enquo(outcome)
  remove_outcome <- FALSE
  # Deal with "..." columns to be ignored
  ignore_columns <- rlang::quos(...)
  ignored <- purrr::map_chr(ignore_columns, rlang::quo_name)
  d_ignore <- NULL
  if (length(ignored)) {
    present <- ignored %in% names(d)
    if (any(!present))
      stop(list_variables(ignored[!present]), " not found in d.")
    if (length(ignored) >= ncol(d))
      stop("You only have ignored columns. Try again.")
    # Separate data into ignored and not
    d_ignore <- dplyr::select(d, !!ignored)
    d <- dplyr::select(d, -dplyr::one_of(ignored))
    # Warn if ignored columns have missingness
    m <- missingness(d_ignore) %>%
      dplyr::filter(percent_missing > 0)
    if (!purrr::is_empty(m$variable))
      warning("These ignored variables have missingness: ",
              list_variables(m$variable))
  }

  # Check global options for factor handling
  opt <- options("contrasts")[[1]][[1]]
  if (opt != "contr.treatment") {
    w <- paste0("Your unordered-factor contrasts option is set to ", opt,
                ". This may produce unexpected behavior, particularly in make_dummies in prep_data. ",
                "Consider resetting it by restarting R, or with: ",
                "options(contrasts = c(\"contr.treatment\", \"contr.poly\"))")
    warning(w)
  }

  # If there's a recipe in recipe, use that
  if (!new_recipe) {
    message("Prepping data based on provided recipe")
    # Look for variables that weren't present in training, add them to ignored
    newvars <- setdiff(names(d), c(recipe$var_info$variable,
                                   attr(recipe, "ignored_columns")))
    if (length(newvars)) {
      warning("These variables were not observed in training ",
              "and will be ignored: ", list_variables(newvars))
      ignored <- c(ignored, newvars)
      d_ignore <- dplyr::bind_cols(d_ignore, dplyr::select(d, !!newvars))
    }

    # Look for predictors unignored in training but missing here and error if not removed by nzv
    missing_vars <- setdiff(recipe$var_info$variable[recipe$var_info$role == "predictor"], names(d))
    if (length(missing_vars))
      warning("These variables were present in training but are missing or ignored here: ",
              list_variables(missing_vars))
    # If data was prepped at all
    if (!is.null(recipe$steps)) {
      # If nzv was done
      if (attr(recipe$steps[[1]], "class")[1] == "step_nzv") {
        # If there were removed columns
        if (length(recipe$steps[[1]]$removals)) {
          # Removed nzv columns should be removed from list. If there are still missing columns, error.
          missing_vars <- missing_vars[missing_vars == length(recipe$steps[[1]]$removals)]
        }
      }
    }
    if (length(missing_vars))
      stop("These variables were present in training but are missing or ignored here: ",
              list_variables(missing_vars))

    # If imputing, look for variables with missingness now that didn't have any in training
    newly_missing <- find_new_missingness(d, recipe)
    if (length(newly_missing))
      warning("The following variable(s) have missingness that was not present when recipe was trained: ",
              list_variables(newly_missing))

    # Outcome gets added as all NAs; set a flag to remove it at end if not in provided DF
    outcome_var <- recipe$var_info$variable[recipe$var_info$role == "outcome"]
    if (length(outcome_var) && !outcome_var %in% names(d))
      remove_outcome <- TRUE

  } else {
    # Look for all-unique characters columns and add them to ignored
    undeclared_ignores <- find_columns_to_ignore(d, c(rlang::quo_name(outcome), ignored))
    if (length(undeclared_ignores)) {
      warning("The following variable(s) look a lot like identifiers: They are ",
              "character-type and have a unique value on every row. They will ",
              "be ignored: ", paste0(undeclared_ignores, collapse = ", "))
      ignored <- c(ignored, undeclared_ignores)
      d_ignore <- dplyr::bind_cols(d_ignore, d[, names(d) %in% undeclared_ignores, drop = FALSE])
      d <- d[, !names(d) %in% undeclared_ignores, drop = FALSE]
    }

    # Initialize a new recipe
    mes <- "Training new data prep recipe"
    ## Start by making all variables predictors...
    ## d gets stored with the recipe and it is the one copy of training data
    ## that we carry around with the model_list
    recipe <- recipes::recipe(d, ~ .)
    recipe$orig_data <- orig_data

    ## Then deal with outcome if present
    if (!rlang::quo_is_missing(outcome)) {
      outcome_name <- rlang::quo_name(outcome)
      if (!outcome_name %in% names(d))
        stop(paste(outcome_name, " not found in d."))
      # Check if there are NAs in the target
      outcome_vec <- dplyr::pull(d, !!outcome)
      # Currently can't deal with logical outcomes
      if (is.logical(outcome_vec) || any(c("TRUE", "FALSE") %in% outcome_vec))
        stop("outcome looks logical. Please convert the outcome to character",
             " with values other than TRUE and FALSE.")
      if (any(is.na(outcome_vec)))
        stop("Found NA values in the outcome column. Clean your data or ",
             "remove these rows before training a model.")
      # Changing the role of outcome from predictor to outcome warns, shush:
      suppressWarnings({
        recipe <- recipes::update_role(recipe, !!outcome, new_role = "outcome")
      })
      # If outcome is binary 0/1, convert to N/Y -----------------------------
      if (factor_outcome && all(outcome_vec %in% 0:1)) {
        if (!is.numeric(outcome_vec))
          stop("factor_outcome is TRUE, but ", outcome_name, " is a character",
               "-type variable with 0s and 1s. Consider making it numeric with ",
               "`as.numeric(as.character())")
        recipe <- recipe %>%
          recipes::step_bin2factor(all_outcomes(), levels = c("Y", "N"))
      }
      mes <- paste0(mes, "...\n")
    } else {
      mes <- paste0(mes, " with no outcome variable specified...\n")
    }
    message(mes)

    # Build recipe step-by-step:

    # TODO
    # Find largely missing columns and convert to factors
    # recipe <- recipe %>% step_hcai_mostly_missing_to_factor()  # nolint

    # Remove columns with near zero variance ----------------------------------
    freq_cut <- 49
    unique_cut <- 10
    if (!is.logical(remove_near_zero_variance)) {
      if (!is.numeric(remove_near_zero_variance))
        stop("remove_near_zero_variance must be logical or numeric for step_nzv")
      if (remove_near_zero_variance < 0 | remove_near_zero_variance > 1)
        stop("remove_near_zero_variance must be numeric between 0 and 1")
      freq_cut <- remove_near_zero_variance ^ -1
      remove_near_zero_variance <- TRUE
    }
    if (remove_near_zero_variance) {
      recipe <- recipe %>%
        recipes::step_nzv(all_predictors(),
                          freq_cut = freq_cut,
                          unique_cut = unique_cut)
    }

    # Check if there are any nominal predictors that won't be removed; stop if not.
    prep_check <- recipes::prep(recipe, training = d)
    removing <- prep_check$steps[[1]]$removals
    vi <- recipe$var_info
    nom_preds <- vi$variable[vi$role == "predictor" & vi$type == "nominal"]
    if (length(nom_preds) && all(nom_preds %in% removing))
      stop("All your categorical columns will be removed because they have ",
           "near-zero variance, which will break prep_data. ",
           "Be less aggressive in removing near-zero variance columns by ",
           "using a larger value of remove_near_zero_variance or setting it ",
           "to FALSE.\n  ",
           list_variables(removing))

    # Convert date columns to useful features and remove original. ------------
    if (!is.character(convert_dates)) {
      if (!is.logical(convert_dates))
        stop('convert_dates must be logical, "none", "continuous", or ',
             '"categories"')
      if (convert_dates)
        convert_dates <- "continuous"
      else
        convert_dates <- "none"
    }
    if (convert_dates %in% c("continuous", "categories")) {
      cols <- find_date_cols(d)
      if (!purrr::is_empty(cols)) {
        recipe <-
          do.call(step_date_hcai,
                  list(recipe = recipe, cols, feature_type = convert_dates)) %>%
          recipes::step_rm(cols)
      }
    } else if (convert_dates == "none") {
      cols <- find_date_cols(d)
      if (!purrr::is_empty(cols))
        recipe <- recipes::step_rm(recipe, cols)
    } else {
      stop('convert_dates must be logical, "none", "continuous", or ',
           '"categories"')
    }

    # Impute ------------------------------------------------------------------
    if (isTRUE(impute)) {
      recipe <- recipe %>%
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
                list_variables(extras),
                ". Available params are: ", list_variables(names(ip)))
      }

      # Impute takes defaults or user specified inputs. Error handling inside.
      recipe <- recipe %>%
        hcai_impute(numeric_method = ip$numeric_method,
                    nominal_method = ip$nominal_method,
                    numeric_params = ip$numeric_params,
                    nominal_params = ip$nominal_params)
    } else if (impute != FALSE) {
      stop("impute must be boolean or list.")
    }

    #Set center and scale to whatever PCA is if PCA is enabled
    if (!(is.numeric(PCA) || is.logical(PCA)))
      stop("PCA must be logical or numeric")
    if (as.logical(PCA)) {
      if (!(as.logical(center) && as.logical(scale))) {
        warning("\"d\" must be centered and scaled to perform PCA. Center and Scale are being set to TRUE.")
        center <- as.logical(PCA)
        scale <- as.logical(PCA)
      }
    }



    # If there are numeric predictors, apply numeric transformations
    var_info <- recipe$var_info
    if (any(var_info$type == "numeric" & var_info$role == "predictor")) {

      # Log transform
      # Saving until columns can be specified



      # Center ------------------------------------------------------------------
      if (isTRUE(as.logical(center))) {
        recipe <- recipe %>%
          recipes::step_center(all_numeric(), - all_outcomes())
      }

      # Scale -------------------------------------------------------------------
      if (isTRUE(as.logical(scale))) {
        recipe <- recipe %>%
          recipes::step_scale(all_numeric(), - all_outcomes())
      }
    }

    # If there are nominal predictors, apply nominal transformations
    if (any(var_info$type == "nominal" & var_info$role == "predictor")) {

      # Add protective levels --------------------------------------------------
      if (add_levels)
        recipe <- step_add_levels(recipe, all_nominal(), - all_outcomes())

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
        recipe <- recipe %>%
          recipes::step_other(all_nominal(), - all_outcomes(),
                              threshold = fac_thresh)
      }

      # Re-add protective levels if missing dropped by step_other
      if (add_levels)
        recipe <- step_add_levels(recipe, all_nominal(), - all_outcomes())

      # make_dummies -----------------------------------------------------------
      if (isTRUE(make_dummies)) {
        make_dummies <- list()
      }
      if (is.list(make_dummies)) {
        recipe <- recipe %>%
          step_dummy_hcai(all_nominal(), - all_outcomes(),
                          levels = make_dummies)
      } else if (!is.logical(make_dummies)) {
        stop("step_dummies must be logical or list")
      }
    }

    #PCA ----------------------------------------------------------------------
    if (as.logical(PCA)) {

      if (!impute && !is.list(impute))
        stop("NAs present in \"d\". PCA not compatible when NAs are present.")

      # Set PCA to default 5 PCs if PCA input was TRUE
      if (is.logical(PCA))
        PCA <- 5

      if (PCA > length(recipes::prep(recipe, training = d)$term_info$role == "predictor"))
        stop("Can't have more components than columns in \"d\".")

      # Perform PCA
      recipe <- recipe %>%
        recipes::step_pca(all_numeric(), -all_outcomes(), num_comp = as.integer(PCA))
    }

    # Prep the newly built recipe ---------------------------------------------
    recipe <- recipes::prep(recipe, training = d)

    # Attach missingness and factor-levels in the original data to the recipe
    attr(recipe, "missingness") <- d_missing
    attr(recipe, "factor_levels") <- d_levels
  }
  # Coerces all logical predictor columns as numeric columns
  if (logical_to_numeric)
    d <- dplyr::mutate_if(d, is.logical, as.numeric)

  # Bake either the newly built or passed-in recipe
  d <- recipes::bake(recipe, d)

  # Find and issue warnings. Perhaps this should be wrapped in if (verbose)
  steps <- map_chr(recipe$steps, ~ attr(.x, "class")[1])
  ## near zero variance
  if ("step_nzv" %in% steps &&
      length(nzv_removed <- recipe$steps[[which(steps == "step_nzv")]]$removals))
    message("Removing the following ", length(nzv_removed), " near-zero variance column(s). ",
            "If you don't want to remove them, call prep_data with ",
            "remove_near_zero_variance as a smaller numeric or FALSE.\n  ",
            list_variables(nzv_removed))

  # Remove outcome if recipe was provided but outcome not present
  if (remove_outcome && outcome_var %in% names(d))
    d <- select_not(d, outcome_var)
  # And remove outcome from original_data_str if it's present
  if (rlang::quo_name(outcome) %in% names(d_ods))
    d_ods <- select_not(d_ods, outcome)
  # Add ignore columns back in and attach as attribute to recipe
  d <- dplyr::bind_cols(d_ignore, d)
  if (new_recipe)
    recipe$template <- dplyr::bind_cols(d_ignore, recipe$template)
  attr(recipe, "ignored_columns") <- unname(ignored)
  attr(recipe, "no_prep") <- no_prep
  attr(d, "recipe") <- recipe
  attr(d, "best_levels") <- best_levels
  attr(d, "original_data_str") <- d_ods
  d <- tibble::as_tibble(d)
  class(d) <- c("prepped_df", class(d))

  return(d)
}

# print method for prep_data
#' @export
print.prepped_df <- function(x, ...) {
  message("healthcareai-prepped data. Recipe used to prepare data:\n")
  print(attr(x, "recipe"))
  message("Current data:\n")
  NextMethod(x)
  return(invisible(x))
}
