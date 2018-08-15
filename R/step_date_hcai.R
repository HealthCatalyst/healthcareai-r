#' Date Feature Generator
#'
#' `step_date_hcai` creates a a *specification* of a recipe
#'  step that will convert date data into one or more factor or
#'  numeric variables. It is a copy of `recipes::step_date` but will
#'  try to guess the date format of columns with the "_DTS" suffix.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'operations for this recipe.
#' @param ... One or more selector functions to choose which
#' variables that will be used to create the new variables. The
#' selected variables should have class `Date` or
#' `POSIXct`. See [selections()] for more details.
#' For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#' role should they be assigned?. By default, the function assumes
#' that the new variable columns created by the original variables
#' will be used as predictors in a model.
#' @param features A character string that includes at least one
#' of the following values: `month`, `dow` (day of week),
#' `doy` (day of year), `week`, `month`,
#' `decimal` (decimal date, e.g. 2002.197), `quarter`,
#' `semester`, `year`.
#' @param label A logical. Only available for features
#' `month` or `dow`. `TRUE` will display the day of
#' the week as an ordered factor of character strings, such as
#' "Sunday." `FALSE` will display the day of the week as a
#' number.
#' @param abbr A logical. Only available for features `month`
#' or `dow`. `FALSE` will display the day of the week as
#' an ordered factor of character strings, such as "Sunday".
#' `TRUE` will display an abbreviated version of the label,
#' such as "Sun". `abbr` is disregarded if `label =
#' FALSE`.
#' @param ordinal A logical: should factors be ordered? Only
#' available for features `month` or `dow`.
#' @param columns A character string of variables that will be
#' used as inputs. This field is a placeholder and will be
#' populated once [prep.recipe()] is used.
#' @param trained A logical to indicate if the number of NA values have been
#' counted in preprocessing.
#' @param skip A logical. Should the step be skipped when the
#' recipe is baked?
#' @return For `step_date_hcai`, an updated version of recipe with
#' the new step added to the sequence of existing steps (if any).
#' For the `tidy` method, a tibble with columns `terms`
#' (the selectors or variables selected), `value` (the feature
#' names), and `ordinal` (a logical).
#' @export
#' @details Unlike other steps, `step_date_hcai` does *not*
#' remove the original date variables. [step_rm()] can be
#' used for this purpose.
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
#' date_values <- bake(date_rec, newdata = examples)
#' date_values
#'
step_date_hcai <- function(recipe, ..., role = "predictor", trained = FALSE,
                           features = "continuous", abbr = TRUE, label = TRUE,
                           ordinal = FALSE, columns = NULL, skip = FALSE) {
  possible_features <- c("categories", "continuous")
  if (!(features %in% possible_features))
    stop("Possible values of `features` should include: ",
         paste0("'", possible_features, "'", collapse = ", "))
  add_step(
    recipe,
    step_date_hcai_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      features = features,
      abbr = abbr,
      label = label,
      ordinal = ordinal,
      columns = columns,
      skip = skip
    )
  )
}

step_date_hcai_new <- function(terms = NULL, role = "predictor",
                               trained = FALSE, features = NULL, abbr = NULL,
                               label = NULL, ordinal = NULL, columns = NULL,
                               skip = FALSE) {
  step(subclass = "date_hcai", terms = terms, role = role, trained = trained,
       features = features, abbr = abbr, label = label, ordinal = ordinal,
       columns = columns, skip = skip)
}

#' @importFrom stats as.formula model.frame
#' @export
prep.step_date_hcai <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  date_data <- info[info$variable %in% col_names, ]

  step_date_hcai_new(terms = x$terms, role = x$role, trained = TRUE,
                     features = x$features, abbr = x$abbr, label = x$label,
                     ordinal = x$ordinal, columns = col_names, skip = x$skip)
}


ord2fac <- function(x, what) {
  x <- getElement(x, what)
  factor(as.character(x), levels = levels(x), ordered = FALSE)
}

convert_to_circular <- function(x, parts, fun) {
  return(fun(2 * pi / parts * x))
}

#' @importFrom lubridate year wday month hour
get_date_features <- function(dt, feats, column_name, abbr = TRUE, label = TRUE,
                              ord = FALSE) {
  if (feats == "continuous") {
    res <- tibble(
      year = year(dt),
      dow_sin = convert_to_circular(wday(dt), 7, sin),
      dow_cos = convert_to_circular(wday(dt), 7, cos),
      month_sin = convert_to_circular(month(dt), 12, sin),
      month_cos = convert_to_circular(month(dt), 12, cos),
      hour_sin = convert_to_circular(hour(dt), 24, sin),
      hour_cos = convert_to_circular(hour(dt), 24, cos)
    )
  } else {
    res <- tibble(
      year = year(dt),
      hour = hour(dt),
      dow = wday(dt, abbr = abbr, label = label),
      month = month(dt, abbr = abbr, label = label)
    )

    if (!ord & label == TRUE) {
      res$dow <- ord2fac(res, "dow")
      res$month <- ord2fac(res, "month")
    }
  }
  names(res) <-
    paste(column_name,
          names(res),
          sep = "_")
  res
}

#' @importFrom tibble as_tibble is_tibble
#' @export
bake.step_date_hcai <- function(object, newdata, ...) {
  # convert to standard date format
  newdata <- convert_date_cols(newdata)

  # get date info for each date column. A list of dataframes.
  date_info <- purrr::map(object$columns, ~{
    get_date_features(
      dt = dplyr::pull(newdata, .x),
      feats = object$features,
      column_name = .x,
      abbr = object$abbr,
      label = object$label,
      ord = object$ordinal
    )
  })

  # combines all cols that exist in each df in the `date_info` list with the
  # cols in newdata
  newdata <- bind_cols(newdata, date_info)

  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}

#' @export
print.step_date_hcai <- function(x, width = max(20, options()$width - 29), ...) {
  cat("Date features from ")
  printer(x$columns, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_date_hcai
#' @param x A `step_date_hcai` object.
#' @export
tidy.step_date_hcai <- function(x, ...) {
  if (x$trained == TRUE) {
    res <- expand.grid(
      terms = x$columns,
      value = x$features,
      ordinal = x$ordinal
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- expand.grid(
      terms = term_names,
      value = x$features,
      ordinal = x$ordinal
    )
  }
  as_tibble(res)
}
