#' @import utils
utils::globalVariables(c("outside", "percent_missing", "variable", ".outcome",
                         ".", "Y", "twoClassSummary"))

printer <- utils::getFromNamespace("printer", "recipes")

sel2char <- utils::getFromNamespace("sel2char", "recipes")
