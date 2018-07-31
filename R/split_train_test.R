#' Split data into training and test data frames
#'
#' @description `split_train_test` splits data into two data frames for
#'   validation of models. One data frame is meant for model training ("train")
#'   and the other is meant to assess model performance ("test"). The
#'   distribution of \code{outcome} will be preserved acrosss the train and test
#'   datasets. Additionally, if there are groups in the dataset, you can keep
#'   all observations within a in the same train/test dataset by passing the
#'   name of the group column to \code{grouping_col}; this is useful, for
#'   example, when there are multiple observations per patient, and you want to
#'   keep each patient within one dataset.
#'
#' @param d Data frame
#' @param outcome Target column, unquoted. Split will be stratified across this
#'   variable
#' @param percent_train Proportion of rows in d to put into training. Default is
#'   0.8
#' @param seed Optional, if provided the function will return the same split
#'   each time it is called
#' @param grouping_col column name that specifies grouping. Individuals in the
#'   same group are in the same training/test set.
#'
#' @return A list of two data frames with names train and test
#' @export
#'
#' @details This function wraps `caret::createDataPartition`. If outcome is a
#'   factor then the test/training porportions are stratified. Otherwise they
#'   are randomly selected.
#'
#'   If the grouping_col is given, then the groups are divided into the test/
#'   training porportions.
#'
#' @examples
#' split_train_test(mtcars, am, .9)
#'
#'
#' # Below is an additional example of grouping. Grouping is where individuals
#' # in the same group are in the same training/test set. Here we group on car
#' # owners. Owners will be in the same training/test set.
#' library(dplyr)
#'
#' mtcars %>%
#'   mutate(owner = rep(letters[1:16], each = 2)) %>%
#'   split_train_test(., am, grouping_col = owner)
#'
split_train_test <- function(d, outcome, percent_train = .8, seed, grouping_col) {
  outcome <- rlang::enquo(outcome)
  if (rlang::quo_is_missing(outcome))
    stop("You must provide an outcome variable to split_train_test.")
  outcome_chr <- rlang::quo_name(outcome)
  if (!outcome_chr %in% names(d))
    stop(outcome_chr, " isn't a column in d.")
  if (!missing(seed))
    set.seed(seed)
  if (missing(grouping_col)) {
    train_rows <- caret::createDataPartition(dplyr::pull(d, !!outcome),
                                             p = percent_train)[[1]]
  } else {
    grouping_col <- rlang::enquo(grouping_col)
    train_rows <- group_strat_split(d, outcome, percent_train, grouping_col)
  }
  return(list(train = d[train_rows, ], test = d[-train_rows, ]))
}

#' Split groups in data training and test data frames
#'
#' @param d Data frame
#' @param outcome Target column, unquoted. Split will be stratified across this
#'   variable
#' @param percent_train Proportion of rows in d to put into training. Default is 0.8
#' @param grouping_col column name that specifies grouping. Individuals in the same
#'   group will be in the same training/test set.
#'
#' @return A list of two data frames with names train and test
#' @noRd
group_strat_split <- function(d, outcome, percent_train = .8, grouping_col) {
  d_limited <- d %>%
    dplyr::group_by(!!grouping_col) %>%
    dplyr::summarize(!!quo_name(outcome) := Mode(!!outcome))

  train_rows_limited <- caret::createDataPartition(
    dplyr::pull(d_limited, !!quo_name(outcome)),
    p = percent_train)[[1]]

  group_values_training <-
    d_limited[train_rows_limited, ] %>%
    dplyr::pull(!!grouping_col)

  train_rows <- which(
    dplyr::pull(d, !!grouping_col) %in%
      group_values_training
  )

  return(train_rows)
}
