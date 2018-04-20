#' Plot variable importance
#'
#' @param x A data frame from \code{\link{get_variable_importance}}
#' @param caption Either "model", "none", or a string to be used as the plot
#'   caption. "model" puts the name of the best-performing model, on which
#'   variable importances are generated, in the caption.
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
get_variable_importance <- function(models) {
  importances <- lapply(models, safe_imp)
  have_imp <- purrr::map_lgl(importances, ~ is.null(.x$error))
  if (!any(have_imp))
    stop("Can't get variable importance for any of these models.")
  model_ranks <- rank_models(models)
  # Use the best model (min rank) where we have variable importance
  use <- which.min(model_ranks[have_imp])
  # When we do multiclass, we'll want to average across cols in imp object
  importances[[use]]$result[[1]][, 1, drop = FALSE] %>%
    tibble::rownames_to_column() %>%
    setNames(c("variable", "importance")) %>%
    dplyr::arrange(desc(importance)) %>%
    as_tibble() %>%
    structure(.,
              model = names(models)[use],
              class = c("variable_importance", class(.)))

}

safe_imp <- purrr::safely(~ caret::varImp(.x))
