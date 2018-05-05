#' Add a good subset of features from high-cardinality variables in a
#' multiple-row-per-observation table
#'
#' @param d Data frame to use in models, at desired grain. Has id and outcome
#' @param longsheet Data frame containing multiple observations per grain. Has
#'   id and groups
#' @param id Name of identifier column, unquoted. Must be present and identical
#'   in both tables
#' @param groups Name of grouping column, unquoted
#' @param outcome Name of outcome column, unquoted
#' @param n_levels Number of levels to return, default = 100. An attempt is made
#'   to return half levels positively associated with the outcome and half
#'   negatively. If n_levels is greater than the number present, all levels will
#'   be returned
#' @param positive_class If classification model, the positive class of the
#'   outcome, default = "Y"; ignored if regression
#' @param levels Character vector of levels to pivot and add. This is a
#'   convenience for when add_best_levels was used in training and the same
#'   columns are desired for deployment.
#' @param fill Passed to \code{\link{pivot}}. Column to be used to fill the
#'   values of cells in the output, perhaps after aggregation by \code{fun}. If
#'   \code{fill} is not provided, counts will be used, as though a fill column
#'   of 1s had been provided.
#' @param fun Passed to \code{\link{pivot}}. Function for aggregation, defaults
#'   to \code{sum}. Custom functions can be used with the same syntax as the
#'   apply family of functions, e.g. \code{fun = function(x)
#'   some_function(another_fun(x))}.
#' @param missing_fill Passed to \code{\link{pivot}}. Value to fill for
#'   combinations of grain and spread that are not present. Defaults to NA, but
#'   0 may be useful as well.
#'
#' @return For \code{add_best_levels}, d with new columns for the best levels
#'   added and an attribute containing the names of levels added. For
#'   \code{get_best_levels}, a character vector of the best levels.
#' @export
#' @seealso \code{\link{pivot}}
#' @description In healthcare, we are often faced with very-high cardinality
#'   categorical variables, where using all of the categories results in very
#'   long model training times and high signal-to-noise. These functions help
#'   identify a subset of categories that are likely to be valuable features.
#'
#'   \code{get_best_levels} finds levels of \code{groups} that are likely to be
#'   useful predictors in \code{d} and returns them as a character vector.
#'   \code{add_best_levels} does the same and adds them, pivoted, to \code{d}.
#'   The function attempts to find both positive and negative predictors of
#'   \code{outcome}.
#'
#'   \code{add_best_levels} stores the best levels in an attribute of the
#'   returned data frame called \code{paste0(groups, "_levels")}, i.e. the name
#'   of the groups column followed by underscore-levels. This is useful in
#'   deployment, to ensure all columns created in training are again created
#'   (see final example).
#'
#'   \code{add_best_levels} accepts arguments that are passed to
#'   \code{\link{pivot}}; however, note that these are not used in determining
#'   the best levels. I.e. \code{get_best_levels} determines which levels are
#'   likely to be good predictors looking only at the distribution of outcome
#'   values for observations where the levels are present or abssent; it does
#'   not use \code{fill} or \code{fun} in this determination. See \code{details}
#'   for more info about how levels are selected.
#'
#' @details Here is how \code{get_best_levels} determines the levels of
#'   \code{groups} that are likely to be good predictors. For regression: For
#'   each group, the difference of the group-mean from the grand-mean is divided
#'   by the standard deviation of the group as a sample (i.e.
#'   centered_mean(group) / sqrt(var(group) / n(group))), and the groups with
#'   the largest absolute values of that statistic are retained. For
#'   classification: For each group, two "log-loss-like" statistics are
#'   calculated. One is log of the fraction of observations in which the group
#'   does not appear. The other is the log of the difference of the proportion
#'   of different outcomes from all the same outcome (e.g. if 4/5 observations
#'   are positive class, this statistic is log(.2)). To ensure retainment of
#'   both positive- and negative-predictors, the all-same-outcome that is used
#'   as the comparison is determined by which side of the median proportion of
#'   positive_class the group falls on. For both regression and classification,
#'   groups that appear in only one record are only retained if there are fewer
#'   than n_levels groups found in more than one record; in this case they are
#'   randomly sampled from.
#'
#' @examples
#' set.seed(45796)
#' df <- tibble::tibble(
#'   patient = paste0("Z", sample(1e6, 5)),
#'   age = sample(20:80, 5),
#'   died = sample(c("N", "Y"), 5, replace = TRUE, prob = c(2, 1))
#' )
#' meds <- tibble::tibble(
#'   patient = sample(df$patient, 10, replace = TRUE),
#'   drug = sample(c("Quinapril", "Vancomycin", "Ibuprofen",
#'                   "Paclitaxel", "Epinephrine", "Dexamethasone"),
#'                 10, replace = TRUE),
#'   dose = sample(c(100, 250), 10, replace = TRUE)
#' )
#' get_best_levels(d = df,
#'                 longsheet = meds,
#'                 id = patient,
#'                 groups = drug,
#'                 outcome = died,
#'                 n_levels = 3)
#' new_df <- add_best_levels(d = df,
#'                           longsheet = meds,
#'                           id = patient,
#'                           groups = drug,
#'                           outcome = died,
#'                           n_levels = 3,
#'                           fill = dose,
#'                           fun = sum,
#'                           missing_fill = 0)
#' new_df
#' # The names of drugs to make columns from are stored in an attribute of new_df
#' # so that the same columns can be used in deployment
#' attr(new_df, "drug_levels")
#' test_df <- tibble::tibble(
#'   patient = "Z12345",
#'   age = 30
#' )
#' test_meds <- tibble::tibble(
#'   patient = rep("Z12345", 2),
#'   drug = rep("Vancomycin", 2),
#'   dose = c(100, 250)
#' )
#' add_best_levels(d = test_df,
#'                 longsheet = test_meds,
#'                 id = patient,
#'                 groups = drug,
#'                 levels = attr(new_df, "drug_levels"),
#'                 fill = dose,
#'                 missing_fill = 0)
add_best_levels <- function(d, longsheet, id, groups, outcome,
                            n_levels = 100, positive_class = "Y", levels = NULL,
                            fill, fun = sum, missing_fill = NA) {
  id <- rlang::enquo(id)
  groups <- rlang::enquo(groups)
  outcome <- rlang::enquo(outcome)
  fill <- rlang::enquo(fill)

  add_as_empty <- character()
  if (is.null(levels)) {
    to_add <- get_best_levels(d, longsheet, !!id, !!groups, !!outcome,
                              n_levels, positive_class)
  } else {
    present_levels <- unique(dplyr::pull(longsheet, !!groups))
    to_add <- levels[levels %in% present_levels]
    add_as_empty <- levels[!levels %in% present_levels]
  }
  longsheet <- dplyr::filter(longsheet, (!!groups) %in% to_add)

  pivot_args <- list(
    d = longsheet,
    grain = eval(id),
    spread = eval(groups),
    fun = fun,
    missing_fill = missing_fill
  )
  if (!missing(fill))
    pivot_args$fill <- eval(fill)
  if (length(add_as_empty))
    pivot_args$extra_cols <- add_as_empty
  pivoted <- do.call(pivot, pivot_args) %>%
    dplyr::left_join(d, ., by = rlang::quo_name(id))
  # Replace any rows not found in the pivot table in the join with missing_fill
  new_cols <- setdiff(names(pivoted), names(d))
  pivoted[new_cols][is.na(pivoted[new_cols])] <- missing_fill
  attr(pivoted, paste0(rlang::quo_name(groups), "_levels")) <- to_add
  return(pivoted)
}

#' @export
#' @rdname add_best_levels
get_best_levels <- function(d, longsheet, id, groups, outcome,
                            n_levels = 100, positive_class = "Y") {
  id <- rlang::enquo(id)
  groups <- rlang::enquo(groups)
  outcome <- rlang::enquo(outcome)
  missing_check(d, outcome)

  tomodel <-
    longsheet %>%
    # Don't want to count the same outcome twice, so only allow one combo of grain x grouper
    dplyr::distinct(!!id, !!groups) %>%
    dplyr::inner_join(d, ., by = rlang::quo_name(id)) %>%
    # Filter any level present in only one grain
    group_by(!!groups) %>%
    filter(n_distinct(!!id) > 1) %>%
    ungroup()

  if (is.numeric(dplyr::pull(tomodel, !!outcome))) {
    # Regression
    # Use the distance from the grand-mean divided by the sample SD to rank predictors
    # Groups with no variance in outcome rise to the top even if they're very small
    tomodel <-
      tomodel %>%
      mutate(!!rlang::quo_name(outcome) := !!outcome - mean(!!outcome)) %>%
      group_by(!!groups) %>%
      summarize(mean_ssd = mean(!!outcome) / sqrt(var(!!outcome) / n())) %>%
      arrange(desc(abs(mean_ssd)))
    tozip <-
      split(tomodel, sign(tomodel$mean_ssd)) %>%
      purrr::map(~ pull(.x, !!groups))
  } else {
    # Classification
    # Using basically the log-loss from being present in all observations and
    # from perfect separation of outcomes. Epislon for present in all is 1/2 a
    # record; for perfect separation is 1/2 an observation.
    total_observations <- n_distinct(dplyr::pull(tomodel, !!id))
    levs <-
      tomodel %>%
      group_by(!!groups) %>%
      summarize(fraction_positive = mean(!!outcome == positive_class),
                # If perfect separation of outcomes, say it got 1/2 of one wrong
                fraction_positive = dplyr::case_when(
                  fraction_positive == 1 ~ 1 - (.5 / total_observations),
                  fraction_positive == 0 ~ .5 / total_observations,
                  TRUE ~ fraction_positive),
                # If level present in every observation, call it every one minus one-half
                present_in = ifelse(n_distinct(!!id) == total_observations,
                                    total_observations - .5, n_distinct(!!id)),
                log_dist_from_in_all = -log(present_in / total_observations)) %>%
      dplyr::select(-present_in)
    median_positive <- median(levs$fraction_positive)
    levs <-
      levs %>%
      mutate(predictor_of = as.integer(fraction_positive > median(fraction_positive)),
             log_loss = - (predictor_of * log(fraction_positive) + (1 - predictor_of) * log(1 - fraction_positive)),
             badness = log_loss * log_dist_from_in_all) %>%
      arrange(badness)
    tozip <-
      split(levs, levs$predictor_of) %>%
      purrr::map(~ pull(.x, !!groups))
  }
  out <- zip_vectors(tozip[[1]], tozip[[2]])
  if (length(out) > n_levels)
    out <- out[seq_len(n_levels)]
  return(out)
}

#' Create one vector from two, with each vectors' first element in the first and
#' second positions, the second elements third and fourth, etc.
#' @noRd
zip_vectors <- function(v1, v2) {
  ll <- list(v1, v2)
  lengths <- purrr::map_int(ll, length)
  zipped <-
    lapply(seq_len(min(lengths)), function(i) {
      c(ll[[1]][i], ll[[2]][i])
    }) %>%
    unlist()
  # if they had different lengths, add trailing part of the longer vector
  if (length(unique(lengths)) > 1)
    zipped <- c(zipped, ll[[which.max(lengths)]][(min(lengths) + 1):max(lengths)])
  return(zipped)
}
