#' Split data into training and test data frames
#'
#' @description `split_train_test` splits data for validation of models. It also
#' offers an implentation to group all observations with the same column value
#' in the same train/test group. This has been created to group patients with
#' multiple observations in the same group.
#'
#' `hcai-impute` adds various imputation methods to an existing
#' recipe. Currently supports mean (numeric only), new_category (categorical
#' only), bagged trees, or knn.
#' @param recipe A recipe object.
#'
#' @param d Data frame
#' @param outcome Target column, unquoted. Split will be stratified across this
#'   variable
#' @param percent_train Proportion of rows in d to put into training. Default is 0.8
#' @param seed Optional, if provided the function will return the same split
#'   each time it is called
#' @param grouping_col column name that specifies grouping. Individuals in the same
#'   group are in the same training/test set.
#'
#' @return A list of two data frames with names train and test
#' @export
#'
#' @details This function wraps `caret::createDataPartition`. If outcome is a factor
#'   then the test/training porportions are stratified. Otherwise they are randomly
#'   selected.
#'
#'   If the grouping_col is given, then the groups are divided into the test/
#'   training porportions.
#'
#' @examples
#' split_train_test(mtcars, am, .9)
#'
#'
#' # splits to training and tests while grouping on the owner of cars
#' mtcars_owners <-
#'   mtcars %>%
#'   mutate(owner = as.factor(rep(letters[1:16], each = 2)))
#'
#' split_train_test(mtcars_owners, am, grouping_col = owner)
#'
#' # Grouping patients
#' split_train_test(d, outcome, grouping_col = patient_id)
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
