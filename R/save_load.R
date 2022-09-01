#' Save models to disk and load models from disk
#'
#' @description Note that model objects contain training data, except columns
#'   ignored (patient_id in the example below). Therefore, if there is PHI in
#'   the training data, the saved model object must be treated as PHI.
#'   \code{save_models} issues a message saying as much.
#'
#' @param x model_list object
#' @param filename File path to save model to or read model from, e.g.
#'   "models/my_models.RDS". Default for \code{save_models} is "models.RDS" in
#'   the working directory (\code{getwd()}). Default for \code{load_models} is
#'   to open a dialog box from which a file can be selected, in which case a
#'   message will issued with code to load the same file without interactivity.
#' @param sanitize_phi Logical. If TRUE (default) training data is removed from
#'   the model object before being saved. Removing training data is important
#'   when sharing models that were trained with data that contain PHI. If
#'   removed, \code{\link{explore}} will not have data to process.
#' @return \code{load_models} returns the model_list which can be assigned to
#'   any variable name
#'
#' @export
#'
#' @examples
#' \donttest{
#' m <- machine_learn(pima_diabetes, patient_id, outcome = diabetes)
#' file <- paste0(tempdir(), "/diabetes_models.RDS")
#' save_models(m, file)
#' # Restart R, move RDS file to another computer, etc.
#' m2 <- load_models(file)
#' all.equal(m, m2)
#' }
save_models <- function(x, filename = "models.RDS", sanitize_phi = TRUE) {
  if (sanitize_phi) {
    attr(x, "recipe")$template <- NULL
    attr(x, "recipe")$orig_data <- NULL
  } else {
    message("The model object being saved contains training data, minus ",
            "ignored ID columns.\nIf there was PHI in training data, normal ",
            "PHI protocols apply to the RDS file.")
  }
  saveRDS(x, filename)
  return(invisible(NULL))
}

#' @rdname save_models
#' @export
load_models <- function(filename) {
  if (missing(filename)) {
    filename <- file.choose()
    mes <- paste0('Loading models. You could automate this with `load_models("',
                  filename, '")`')
    message(mes)
  }
  x <- readRDS(filename)
  attr(x, "loaded_from_rds") <- filename
  if (!is.null(attr(x, "recipe")$template) | !is.null(attr(x, "recipe")$orig_data))
    message("*** If there was PHI in training data, normal PHI protocols apply",
            " to this model object. ***")
  return(x)
}
