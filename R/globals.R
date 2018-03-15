#' @import utils
utils::globalVariables(c("outside", "percent_missing", "variable", ".outcome",
                         ".", "Y", "twoClassSummary",
                         "w_cc", "w_ccmcc"))

printer <- utils::getFromNamespace("printer", "recipes")

sel2char <- utils::getFromNamespace("sel2char", "recipes")
