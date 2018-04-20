#' @import utils
#' @import methods
utils::globalVariables(c("outside", "percent_missing", "variable", ".outcome",
                         ".", "Y", "twoClassSummary", "rowIndex",
                         "importance", "caret", "w_cc", "w_ccmcc",
                         "fill_ones", "guessed_formats", "has_guesses",
                         "first_entry", "converted_date", "is_valid",
                         "any_valid"))

printer <- utils::getFromNamespace("printer", "recipes")

sel2char <- utils::getFromNamespace("sel2char", "recipes")

ellipse_check <- utils::getFromNamespace("ellipse_check", "recipes")
