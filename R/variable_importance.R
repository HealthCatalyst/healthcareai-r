#' Plot variable importance
#'
#' @param x A data frame from \code{\link{get_variable_importance}}
#' @param title Either "model", "none", or a string to be used as the plot
#'   caption. "model" puts the name of the best-performing model, on which
#'   variable importances are generated, in the title.
#' @param max_char Maximum length of variable names to leave untruncated.
#'   Default = 40; use \code{Inf} to prevent truncation. Variable names longer
#'   than this will be truncated to leave the beginning and end of each variable
#'   name, bridged by " ... ".
#' @param caption Plot title
#' @param font_size Relative size for all fonts, default = 11
#' @param point_size Size of dots, default = 3
#' @param print Print the plot?
#' @param ... Unused
#'
#' @return A ggplot object, invisibly.
#' @export
#' @importFrom stats reorder
#'
#' @examples
#' machine_learn(pima_diabetes[1:50, ], patient_id, outcome = diabetes, tune = FALSE) %>%
#'   get_variable_importance() %>%
#'   plot()
plot.variable_importance <- function(x,
                                     title = "model",
                                     max_char = 40,
                                     caption = NULL,
                                     font_size = 11,
                                     point_size = 3,
                                     print = TRUE,
                                     ...) {

  if (!is.data.frame(x))
    stop("x must be a data frame from get_variable_importance, or at least look like one!")
  if (any(names(x) != c("variable", "importance")))
    stop("x must be a data frame from get_variable_importance, or at least look like one!")

  if (title == "model") {
    title <- paste(attr(x, "model"), "variable importance")
  } else if (title == "none") {
    title <- NULL
  }

  x$variable <- trunc_char(x$variable, max_char)

  the_plot <-
    x %>%
    filter(importance > 0) %>%
    ggplot(aes(x = reorder(variable, importance), y = importance)) +
    geom_point(size = point_size) +
    coord_flip() +
    labs(title = title, caption = caption) +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = "Relative Importance", limits = c(0, 100)) +
    theme_gray(base_size = font_size)

  if (print)
    print(the_plot)
  return(invisible(the_plot))
}

#' Get variable importances
#'
#' @param models model_list object
#' @param remove_zeros Remove features with zero variable importance? Default is
#'   TRUE
#' @param top_n Integer: How many variables to return? The top_n most important
#'   variables be returned. If missing (default), all variables are returned
#'
#' @return Data frame of variables and their importance for predictive power
#' @export
#'
#' @details Some algorithms provide variable importance, others don't. The
#'   best-performing model that offers variable importance will be used.
#'
#' @seealso \code{\link{plot.variable_importance}}
#'
#' @examples
#' m <- machine_learn(mtcars, outcome = mpg, models = "rf", tune = FALSE)
#' (vi <- get_variable_importance(m))
#' plot(vi)
get_variable_importance <- function(models, remove_zeros = TRUE, top_n) {

  use <- choose_vi_model(models)
  imp <- caret::varImp(models[[use]])
  imp <-
    imp[[1]][, 1, drop = FALSE] %>%
    tibble::rownames_to_column() %>%
    setNames(c("variable", "importance")) %>%
    dplyr::arrange(desc(importance)) %>%
    as_tibble()

  if (remove_zeros)
    imp <- dplyr::filter(imp, importance != 0)

  if (!missing(top_n)) {
    if (top_n < nrow(imp))
      imp <- imp[seq_len(top_n), ]
  }

  structure(imp,
            model = use,
            class = c("variable_importance", class(imp)))
}

# This is where we keep which models can really do varible importance (glmnet)
# can, but it's values are bogus because coefficients aren't standardized.
# This should eventually go into a master model table (issue #1113).
choose_vi_model <- function(models, have_vi = c("Random Forest",
                                                "eXtreme Gradient Boosting")) {
  usable <- intersect(have_vi, names(models))
  if (!length(usable))
    stop("Can't get variable importance from ", list_variables(names(models)))

  original_best <- best_model <- extract_model_info(models)$best_model_name
  # If the best model doesn't have VI, remove it and check again
  while (!best_model %in% have_vi) {
    models <- models[!names(models) %in% best_model]
    best_model <- extract_model_info(models)$best_model_name
  }
  use <- best_model

  if (original_best != use)
    warning(original_best, " was the best performing model and will be used to ",
            "make predictions, but it doesn't offer variable importance. ",
            "Returning variable importance from ", use, " instead. ",
            "To make predictions from ", use, ", use: models['", use, "'].")

  return(use)
}
