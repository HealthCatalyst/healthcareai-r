#' Split data into training and test data frames
#'
#' @param d Data frame
#' @param outcome Target column, unquoted. Split will be stratified across this
#'   variable
#' @param percent_train Proportion of rows in d to put into training. Default is 0.8
#' @param seed Optional, if provided the function will return the same split
#'   each time it is called
#' @param grouping_col column header of dataframe to base grouping split
#'
#' @return A list of two data frames with names train and test
#' @export
#'
#' @details This function wraps `caret::createDataPartition`. If outcome is a factor
#'   then the test/training porportions are stratified. Otherwise they are randomly 
#'   selected. If the grouping_col is given, then groups are kept together where possible.
#'   If grouping is prefered over stratified split, then make sure that outcome is
#'   not a factor.
#'
#' @examples
#' split_train_test(mtcars, am, .9)
split_train_test <- function(d, outcome, percent_train = .8, seed, grouping_col) {
  outcome <- rlang::enquo(outcome)
  if (rlang::quo_is_missing(outcome))
    stop("You must provide an outcome variable to tune_models.")
  outcome_chr <- rlang::quo_name(outcome)
  if (!outcome_chr %in% names(d))
    stop(outcome_chr, " isn't a column in d.")
  if (!missing(seed))
    set.seed(seed)
  if (!missing(grouping_col)){
    train_rows <- sample.split(dplyr::pull(d, !!outcome),
                               SplitRatio = percent_train,
                               group = grouping_col)
    return(list(train = d[train_rows, ], test = d[!train_rows, ]))
  } else {
    train_rows <- caret::createDataPartition(dplyr::pull(d, !!outcome),
                                             p = percent_train)[[1]]
    return(list(train = d[train_rows, ], test = d[-train_rows, ]))
  }
}
