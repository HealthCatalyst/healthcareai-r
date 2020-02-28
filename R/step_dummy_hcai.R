#' Dummy Variables Creation
#'
#' @description \code{step_dummy_hcai} creates a *specification* of a recipe step
#'  that will convert nominal data (e.g. character or factors) into one or more
#'  numeric binary model terms for the levels of the original data. Various
#'  portions of this step are copied from \code{recipes::step_dummy}. Beyond
#'  original \code{recipes::step_dummy} implementation, this step sets reference
#'  levels to provided reference levels or mode.
#'
#' @inheritParams recipes::step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to create the dummy variables. See
#'  [selections()] for more details. The selected
#'  variables must be factors. For the \code{tidy} method, these are
#'  not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the binary dummy variable columns created by the original
#'  variables will be used as predictors in a model.
#' @param naming A function that defines the naming convention for
#'  new dummy columns. See Details below.
#' @param levels A list that provides the ordered levels of nominal variables.
#'  If all the unique values in a nominal variable are not included, the
#'  remaining values will be added to the given levels. The first level will be
#'  listed as the \code{ref_level} attribute for the step object. If levels are
#'  not provided for a nominal variable, the mode value will be used as the
#'  reference level.
#' @return An updated version of \code{recipe} with the new step
#'  added to the sequence of existing steps (if any). For the
#'  \code{tidy} method, a tibble with columns \code{terms} (the
#'  selectors or variables selected).
#' @keywords datagen
#' @export
#' @details \code{step_dummy_hcai} will create a set of binary dummy
#'  variables from a factor variable. For example, if an unordered
#'  factor column in the data set has levels of "red", "green",
#'  "blue", the dummy variable bake will create two additional
#'  columns of 0/1 data for two of those three values (and remove
#'  the original column). For ordered factors, polynomial contrasts
#'  are used to encode the numeric values.
#'
#' By default, the excluded dummy variable (i.e. the reference
#'  cell) will correspond to the first level of the unordered
#'  factor being converted.
#'
#' The function allows for non-standard naming of the resulting
#'  variables. For an unordered factor named `x`, with levels `"a"`
#'  and `"b"`, the default naming convention would be to create a
#'  new variable called `x_b`. Note that if the factor levels are
#'  not valid variable names (e.g. "some text with spaces"), it will
#'  be changed by [base::make.names()] to be valid (see the example
#'  below). The naming format can be changed using the `naming`
#'  argument and the function [dummy_names()] is the default. This
#'  function will also change the names of ordinal dummy variables.
#'  Instead of values such as "`.L`", "`.Q`", or "`^4`", ordinal
#'  dummy variables are given simple integer suffixes such as
#'  "`_1`", "`_2`", etc.
#'
#' To change the type of contrast being used, change the global
#' contrast option via `options`.
#'
#' When the factor being converted has a missing value, all of the
#'  corresponding dummy variables are also missing.
#'
#' When data to be processed contains novel levels (i.e., not
#' contained in the training set), a missing value is assigned to
#' the results. See [step_other()] for an alternative.
#'
#' The [package vignette for dummy variables](
#' https://topepo.github.io/recipes/articles/Dummies.html)
#' and interactions has more information.
#'
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [dummy_names()], [step_regex()], [step_count()],
#'  [step_ordinalscore()], [step_unorder()], [step_other()]
#'  [step_novel()]
#' @examples
#' rec <- recipes::recipe(head(pima_diabetes), ~.) %>%
#'   healthcareai:::step_dummy_hcai(weight_class)
#' d <- recipes::prep(rec, training = pima_diabetes)
#' d <- recipes::bake(d, new_data = pima_diabetes)
#'
#' # Specify ref_levels
#' ref_levels <- list(weight_class = "normal")
#' rec <- recipes::recipe(head(pima_diabetes), ~.)
#' rec <- rec %>% healthcareai:::step_dummy_hcai(weight_class,
#'                                               levels = ref_levels)
#'
step_dummy_hcai <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           naming = dummy_names,
           levels = NULL,
           skip = FALSE,
           id = rand_id("bagimpute")) {
    add_step(
      recipe,
      step_dummy_hcai_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        naming = naming,
        levels = levels,
        ref_levels = NULL,
        dummies = NULL,
        skip = skip,
        id = id
      )
    )
  }

step_dummy_hcai_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           naming = naming,
           levels = levels,
           ref_levels = ref_levels,
           dummies = dummies,
           skip = FALSE,
           id
  ) {
    step(
      subclass = "dummy_hcai",
      terms = terms,
      role = role,
      trained = trained,
      naming = naming,
      levels = levels,
      ref_levels = ref_levels,
      dummies = dummies,
      skip = skip,
      id = id
    )
  }

#' @importFrom stats as.formula model.frame na.pass
#' @importFrom dplyr bind_cols
#' @export
prep.step_dummy_hcai <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  fac_check <-
    vapply(training[, col_names], is.factor, logical(1))
  if (any(!fac_check))
    stop(
      "The following variables are not factor vectors: ",
      paste0("`", names(fac_check)[!fac_check], "`", collapse = ", "),
      call. = FALSE
    )

  # I hate doing this but currently we are going to have
  # to save the terms object from the original (= training)
  # data
  levels <- vector(mode = "list", length = length(col_names))
  names(levels) <- col_names
  ref_levels <- character()
  for (i in seq_along(col_names)) {
    training_col <- getElement(training, col_names[i])

    # See if the given levels are in the given column
    existing_levels <- levels(training_col) %in% x$levels[[col_names[i]]]
    if (any(existing_levels) &&
        x$levels[[col_names[i]]][1] %in% levels(training_col)) {
      # Add the remaining levels that were not given by user
      x$levels[[col_names[i]]] <- c(x$levels[[col_names[i]]],
                                    levels(training_col)[!existing_levels])
    } else {
      # If given levels are not present in given feature, set the reference
      # level to the mode, and then add remaining levels
      training_mode <- as.character(Mode(training_col))
      training_levels <- levels(training_col)
      x$levels[[col_names[i]]] <- c(training_mode, training_levels[
        -which(training_levels == training_mode)
      ])
    }

    form_chr <- paste0("~", col_names[i])
    form <- as.formula(form_chr)
    suppressWarnings(
      terms <- model.frame(form,
                           data = training,
                           xlev = x$levels[col_names[i]],
                           na.action = na.pass)
    )

    levels[[i]] <- attr(terms, "terms")

    ## About factor levels here: once dummy variables are made,
    ## the `stringsAsFactors` info saved in the recipe (under
    ## recipe$levels will remove the original record of the
    ## factor levels at the end of `prep.recipe` since it is
    ## not a factor anymore. We'll save them here and reset them
    ## in `bake.step_dummy_hcai` just prior to calling `model.matrix`


    factor_levels <- x$levels[[col_names[i]]]
    attr(levels[[i]], "values") <- factor_levels

    ## Create a table of all the variables that will be created
    tmp_levels <- stringr::str_replace_all(factor_levels, " ", ".")
    new_dummies <- tibble(
      feature = col_names[i],
      dummy = paste(col_names[i], tmp_levels[-1], sep = "_"),
      ref = tmp_levels[1]
    )
    dummies <-
      if (i == 1) {
        new_dummies
      } else {
        dummies %>%
          bind_rows(new_dummies)
      }
    ref_levels[i] <- tmp_levels[1]
  }
  names(ref_levels) <- col_names

  step_dummy_hcai_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    naming = x$naming,
    levels = levels,
    ref_levels = ref_levels,
    dummies = dummies,
    skip = x$skip,
    id = x$id
  )
}

warn_new_levels <- function(dat, lvl) {
  ind <- which(!(dat %in% lvl))
  if (length(ind) > 0) {
    lvl2 <- unique(dat[ind])
    warning("There are new levels in a factor: ",
            paste0(lvl2, collapse = ", "),
            call. = FALSE)
  }
  invisible(NULL)
}

#' @export
bake.step_dummy_hcai <- function(object, new_data, ...) {
  ## Maybe do this in C?
  col_names <- names(object$levels)

  ## `na.action` cannot be passed to `model.matrix` but we
  ## can change it globally for a bit
  old_opt <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = old_opt))

  for (i in seq_along(object$levels)) {
    # Make sure that the incoming data has levels consistent with
    # the original (see the note above)
    orig_var <- names(object$levels)[i]
    fac_type <- attr(object$levels[[i]], "dataClasses")

    if (!any(names(attributes(object$levels[[i]])) == "values"))
      stop("Factor level values not recorded", call. = FALSE)

    warn_new_levels(
      new_data[[orig_var]],
      attr(object$levels[[i]], "values")
    )

    new_data[, orig_var] <-
      factor(getElement(new_data, orig_var),
             levels = attr(object$levels[[i]], "values"),
             ordered = fac_type == "ordered")

    indicators <-
      model.frame(
        as.formula(paste0("~", orig_var)),
        data = new_data[, orig_var],
        xlev = attr(object$levels[[i]], "values"),
        na.action = na.pass
      )

    indicators <-
      model.matrix(
        object = object$levels[[i]],
        data = indicators
      )
    indicators <- as_tibble(indicators)

    options(na.action = old_opt)
    on.exit(expr = NULL)

    indicators <- indicators[, colnames(indicators) != "(Intercept)",
                             drop = FALSE]

    ## use backticks for nonstandard factor levels here
    used_lvl <- gsub(paste0("^", col_names[i]), "", colnames(indicators))
    colnames(indicators) <- object$naming(col_names[i], used_lvl,
                                          fac_type == "ordered")
    new_data <- bind_cols(new_data, as_tibble(indicators))
    new_data[, col_names[i]] <- NULL
  }
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

#' @export
print.step_dummy_hcai <-
  function(x, width = max(20, options()$width - 20), ...) {
    if (x$trained) {
      cat("Dummy variables from ")
      cat(list_variables(names(x$levels)))
    } else {
      cat("Dummy variables from ")
      # Not trained yet. Use selector quosures to describe which features will
      # be dummified.
      cat(list_variables(map_chr(x$terms, quo_name)))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname step_dummy_hcai
#' @param x A `step_dummy_hcai` object.
#' @export
#' @export tidy.step_dummy_hcai
tidy.step_dummy_hcai <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$levels), id = x$id)
  } else {
    res <- tibble(terms = sel2char(x$terms), id = x$id)
  }
  res
}
