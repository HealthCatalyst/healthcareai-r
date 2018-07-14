#' Split data into training and test data frames
#'
#' @param d Data frame
#' @param outcome Target column, unquoted. Split will be stratified across this
#'   variable
#' @param percent_train Proportion of rows in d to put into training. Default is 0.8
#' @param seed Optional, if provided the function will return the same split
#'   each time it is called
#' @param grouping_col column name that specifies grouping. Individuals in the same
#'   group are in the same training/test set.
#' @param aggreg_func when the data is grouped, the outcome column is aggregated
#'   based on this function.
#'
#' @return A list of two data frames with names train and test
#' @export
#'
#' @details This function wraps `caret::createDataPartition`. If outcome is a factor
#'   then the test/training porportions are stratified. Otherwise they are randomly
#'   selected. 
#'   
#'   If the grouping_col is given, then the groups are divided into the test/
#'   training porportions by using the given aggregate function.
#'
#' @examples
#' split_train_test(mtcars, am, .9)
split_train_test <- function(d, outcome, percent_train = .8, seed, grouping_col = NULL, aggreg_func = dplyr::first) {
  outcome <- rlang::enquo(outcome)
  if (rlang::quo_is_missing(outcome))
    stop("You must provide an outcome variable to tune_models.")
  outcome_chr <- rlang::quo_name(outcome)
  if (!outcome_chr %in% names(d))
    stop(outcome_chr, " isn't a column in d.")
  if (!missing(seed))
    set.seed(seed)
  if (!missing(grouping_col)){
    return(
      group_strat_split(d, outcome, percent_train, grouping_col, aggreg_func)
    )
  } else {
    train_rows <- caret::createDataPartition(dplyr::pull(d, !!outcome),
                                             p = percent_train)[[1]]
    return(list(train = d[train_rows, ], test = d[-train_rows, ]))
  }
}

#' Split groups in data training and test data frames
#'
#' @param d Data frame
#' @param outcome Target column, unquoted. Split will be stratified across this
#'   variable
#' @param percent_train Proportion of rows in d to put into training. Default is 0.8
#' @param grouping_col column name that specifies grouping. Individuals in the same
#'   group are in the same training/test set.
#' @param aggreg_func when the data is grouped, the outcome column is aggregated
#'   based on this function.
#'
#' @return A list of two data frames with names train and test
group_strat_split <- function(d, outcome, percent_train = .8, grouping_col, aggreg_func = dplyr::first) {
  grouping_col <- rlang::enquo(grouping_col)
  outcome <- rlang::enquo(outcome)
  d_limited <- d %>% dplyr::group_by(!!grouping_col) %>% dplyr::summarize(outcome = aggreg_func(!!outcome))

  outcome <- rlang::quo(outcome)
  col_insterest <- d_limited %>% dplyr::pull(!!outcome)

  train_rows <- caret::createDataPartition(col_insterest, p = percent_train)[[1]]
  
  group_training <- d_limited[train_rows, ] %>% dplyr::pull(!! grouping_col)

  training_data <- dplyr::filter(d, !! grouping_col %in% group_training)
  test_data <- dplyr::filter(d, ! (!! grouping_col %in% group_training))
  return(list(train = training_data, test = test_data))
}
