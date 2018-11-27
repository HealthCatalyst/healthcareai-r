#' Date and Time Feature Generator
#'
#' @description `step_date_hcai` creates a *specification* of a recipe step that
#'  will convert date data into factor or numeric variable(s). This step will
#'  guess the date format of columns with the "_DTS" suffix, and then create
#'  either `categories` or `continuous` columns. Various portions of this step
#'  are copied from `recipes::step_date`.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables that will
#'   be used to create the new variables. The selected variables should have
#'   class `Date` or `POSIXct` or their name must end with `DTS`. See
#'   [selections()] for more details. For the `tidy` method, these are not
#'   currently used.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the function assumes that the new variable
#'   columns created by the original variables will be used as predictors in a
#'   model.
#' @param feature_type character, either `continuous` (default) or `categories`.
#' @param columns A character string of variables that will be used as inputs.
#'   This field is a placeholder and will be populated once [prep.recipe()] is
#'   used.
#' @param trained A logical to indicate if the number of NA values have been
#'   counted in preprocessing.
#' @param skip A logical. Should the step be skipped when the recipe is baked?
#' @param id a unique step id that will be used to unprep
#' @return For `step_date_hcai`, an updated version of recipe with the new step
#'   added to the sequence of existing steps (if any). For the `tidy` method, a
#'   tibble with columns `terms` (the selectors or variables selected), `value`
#'   (the feature names), and `ordinal` (a logical).
#' @export
#' @details Unlike other steps, `step_date_hcai` does *not* remove the original
#'   date variables. [step_rm()] can be used for this purpose.
#' @examples
#' library(lubridate)
#' library(recipes)
#'
#' examples <- data.frame(Dan = ymd("2002-03-04") + days(1:10),
#'                        Stefan = ymd("2006-01-13") + days(1:10))
#' date_rec <- recipe(~ Dan + Stefan, examples) %>%
#'   step_date_hcai(all_predictors())
#'
#' date_rec <- prep(date_rec, training = examples)
#'
#' date_values <- bake(date_rec, new_data = examples)
#' date_values
#'
#' # changing `feature_type` to `categories`
#' date_rec <-
#'   recipe(~ Dan + Stefan, examples) %>%
#'   step_date_hcai(all_predictors(), feature_type = "categories")
#'
#' date_rec <- prep(date_rec, training = examples)
#'
#' date_values <- bake(date_rec, new_data = examples)
#' date_values
step_date_hcai <- function(recipe, ..., role = "predictor", trained = FALSE,
                           feature_type = "continuous", columns = NULL,
                           skip = FALSE,
                           id = rand_id("bagimpute")) {
  possible_feature_types <- c("categories", "continuous")
  if (!(feature_type %in% possible_feature_types))
    stop("Possible values of `feature_type` should include: ",
         paste0("'", possible_feature_types, "'", collapse = ", "))
  add_step(
    recipe,
    step_date_hcai_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      feature_type = feature_type,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_date_hcai_new <- function(terms = NULL, role = "predictor",
                               trained = FALSE, feature_type = NULL,
                               columns = NULL, skip = FALSE,
                               id) {
  step(subclass = "date_hcai", terms = terms, role = role, trained = trained,
       new_features = NULL, feature_type = feature_type, columns = columns,
       skip = skip, id = id)
}

#' @importFrom stats as.formula model.frame
#' @export
prep.step_date_hcai <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  date_data <- info[info$variable %in% col_names, ]

  step_date_hcai_new(terms = x$terms, role = x$role, trained = TRUE,
                     feature_type = x$feature_type, columns = col_names,
                     skip = x$skip, id = x$id)
}


ord2fac <- function(x, what) {
  x <- getElement(x, what)
  factor(as.character(x), levels = levels(x), ordered = FALSE)
}

convert_to_circular <- function(x, parts, fun) {
  return(fun(2 * pi / parts * x))
}

#' @importFrom lubridate year wday month hour
get_date_features <- function(dt, feats, column_name) {
  if (feats == "continuous") {
    res <- tibble(
      dow_sin = convert_to_circular(wday(dt), 7, sin),
      dow_cos = convert_to_circular(wday(dt), 7, cos),
      month_sin = convert_to_circular(month(dt), 12, sin),
      month_cos = convert_to_circular(month(dt), 12, cos),
      year = year(dt)
    )

    # Only make hour feature if time exists in object
    if (is.POSIXt(dt)) {
      res <-
        res %>%
        mutate(
          hour_sin = convert_to_circular(hour(dt), 24, sin),
          hour_cos = convert_to_circular(hour(dt), 24, cos)
        )
    }
  } else {
    res <- tibble(
      dow = wday(dt, abbr = TRUE, label = TRUE),
      month = month(dt, abbr = TRUE, label = TRUE),
      year = year(dt)
    )

    res <-
      res %>%
      mutate(
        dow = ord2fac(res, "dow"),
        month = ord2fac(res, "month")
      )

    # Only make hour feature if time exists in object
    if (is.POSIXt(dt))
      res <-
        res %>%
        mutate(hour = hour(dt))
  }
  names(res) <-
    paste(column_name,
          names(res),
          sep = "_")
  res
}

#' @importFrom tibble as_tibble is_tibble
#' @export
bake.step_date_hcai <- function(object, new_data, ...) {
  # convert to standard date format
  new_data <- convert_date_cols(new_data)

  # get date info for each date column. A list of dataframes.
  date_info <- purrr::map(object$columns, ~{
    get_date_features(
      dt = dplyr::pull(new_data, .x),
      feats = object$feature_type,
      column_name = .x
    )
  })

  # combines all cols that exist in each df in the `date_info` list with the
  # cols in new_data
  new_data <- dplyr::bind_cols(new_data, date_info)

  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

#' @export
print.step_date_hcai <- function(x, width = max(20, options()$width - 29),
                                 ...) {
  cat("Date features from ")
  printer(x$columns, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_date_hcai
#' @param x A `step_date_hcai` object.
#' @export
#' @export tidy.step_date_hcai
tidy.step_date_hcai <- function(x, ...) {
  if (x$trained == TRUE) {
    res <- expand.grid(
      terms = x$columns,
      feature_type = x$feature_type,
      id = x$id
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- expand.grid(
      terms = term_names,
      feature_type = x$feature_type,
      id = x$id
    )
  }
  as_tibble(res)
}
