#' Plot variable importance
#'
#' @param x A data frame from \code{\link{get_variable_importance}}
#' @param caption Either "model", "none", or a string to be used as the plot
#'   caption. "model" puts the name of the best-performing model, on which
#'   variable importances are generated, in the caption.
#' @param max_char Maximum length of variable names to leave untruncated.
#'   Default = 40; use \code{Inf} to prevent truncation. Variable names longer
#'   than this will be truncated to leave the beginning and end of each variable
#'   name, bridged by " ... ".
#' @param title Plot title
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
                                     caption = "model",
                                     max_char = 40,
                                     title = NULL,
                                     font_size = 11,
                                     point_size = 3,
                                     print = TRUE,
                                     ... ) {

  if ( (is.data.frame(x) && names(x) != c("variable", "importance") ) ||
       !is.data.frame(x))
    stop("x must be a data frame from get_variable_importance, or at least look like one!")

  if (caption == "model") {
    caption <- paste(attr(x, "model"), "variable importance")
  } else if (caption == "none") {
    caption <- NULL
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
#' m <- flash_models(mtcars, outcome = mpg, models = "rf")
#' get_variable_importance(m)
get_variable_importance <- function(models, remove_zeros = TRUE, top_n) {
  # Currently RF is the only alg with variable importance we want to use
  use <- "Random Forest"
  if (!use %in% names(models))
    stop("Can't get variable importance for any of these models.")
  best_model <- extract_model_info(models)$best_model_name
  if (best_model != use)
    warning("Returning ", use, " variable importance, but ", best_model, " was the ",
            "best performing model and will be used to make predictions. To ",
            "make predictions from ", use, " instead, use: models['", use, "'].")
  imp <- caret::varImp(models$`Random Forest`)
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
