#' @import utils
#' @import methods
utils::globalVariables(c("outside", "percent_missing", "variable", ".outcome",
                         ".", "Y", "twoClassSummary",
                         "w_cc", "w_ccmcc",
                         "fill_ones"))

printer <- utils::getFromNamespace("printer", "recipes")

sel2char <- utils::getFromNamespace("sel2char", "recipes")
