#' Save models to disk and load models from disk
#'
#' @description Note that model objects contain training data, except columns
#'   ignored (patient_id in the example below). Therefore, if there is PHI in
#'   the training data, the saved model object must be treated as PHI.
#'   \code{save_models} issues a message saying as much.
#'
#' @param x model_list object
#' @param filename File path to save model to or read model from, e.g.
#'   "models/my_models.RDS". If not given, a dialog box to choose file location
#'   will be provided.
#' @return load_models returns the model_list which can be assigned to any
#'   variable name
#'
#' @export
#' @importFrom tcltk tclvalue tkgetSaveFile
#'
#' @examples
#' \dontrun{
#' m <- machine_learn(pima_diabetes, patient_id, outcome = diabetes)
#' save_models(m, "diabetes_models.RDS")
#' # Restart R, move RDS file to another computer, etc.
#' m2 <- load_models("diabetes_models.RDS")
#' all.equal(m, m2)
#' }
save_models <- function(x, filename) {
  if (missing(filename)) {
    filename <-
      tkgetSaveFile(title = "Save models...",
                    initialfile = "models.RDS",
                    defaultextension = ".RDS") %>%
      tclvalue()
    mes <- paste0("Saving models. You could automate this with `save_models(",
                  deparse(substitute(x)), ", ", filename, ")`")
    message(mes)
  }
  saveRDS(x, filename)
  message("The model object being saved contains training data, minus ignored ID columns.\n",
          "If there was PHI in training data, normal PHI protocols apply to the RDA file.")
  return(invisible(NULL))
}

#' @rdname save_models
#' @export
load_models <- function(filename) {
  if (missing(filename)) {
    filename <- file.choose()
    mes <- paste0("Loading models. You could automate this with `load_models(",
                  filename, ")`")
    message(mes)
  }
  x <- readRDS(filename)
  return(x)
}
