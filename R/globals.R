#' @import utils
#' @import methods
utils::globalVariables(c("outside", "percent_missing", "variable", ".outcome",
                         ".", "Y", "twoClassSummary", "rowIndex",
                         "importance", "caret", "w_cc", "w_ccmcc",
                         "fill_ones", "guessed_formats", "has_guesses",
                         "first_entry", "converted_date", "is_valid",
                         "any_valid", "badness", "best_levels", "fraction_positive",
                         "log_dist_from_in_all", "log_loss", "mean_ssd",
                         "predictor_of", "present_in", "original_data_str",
                         "coefficient", "model", "obs"))


printer <- utils::getFromNamespace("printer", "recipes")

sel2char <- utils::getFromNamespace("sel2char", "recipes")

ellipse_check <- utils::getFromNamespace("ellipse_check", "recipes")
