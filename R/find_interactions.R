#' Calculates the h-statistic for either all combinations or for the most
#' significant interactions.
#' @param d Dataframe or tibble
#' @param outcome Unquoted column name that indicates the target variable.
#' @param brute_force Logical. When TRUE (default) every combination of
#'   variables will be searched. If FALSE, a greedy implementation will be used.
#'   If greedy is used hidden interactions will not be found.
#' @param order Integer (default = 3) specifying how many orders to search for
#'   in the data set. This function is designed for 2 or 3 orders.
#' @param random_seed Numeric. Sets the random seed for the gbm model. If not
#'   provided, models will be randomized.
#' @param verbose Logical. If TRUE (default) will list the amount of time it
#'   takes to run brute_force.
#' @param greedy_cutoff Numeric. If brute_force is FALSE, all H-statistics above
#'   this threshold will not be explored for further interactions.
#'
#' @details get_interactions is dependent an tidyverse, gbm, recipes, and rlang.
#'   This does not support data columns of type character or date. This function
#'   also doesn't support columns that only contain missing values. Currently
#'   supports regression and binomial output.
#' @import gbm
#' @export
#' @examples
#' library(dplyr)
#' d <-
#'   pima_diabetes %>%
#'   mutate_if(is.character, as.factor)
#'
#'
#' # finds interactions for binomial classification
#' interactions <- find_interactions(d, diabetes)
#'
#' # finds interactions for regression classification
#' interactions <- find_interactions(d, pregnancies)
#'
#' # Choosing the greedy instead
#' interactions <- find_interactions(d, pregnancies, brute_force = FALSE)
#'
#' # Choosing the threshold for the greedy
#' interactions <- find_interactions(d, pregnancies, brute_force = FALSE,
#'                                   greedy_cutoff = .02)
find_interactions <- function(d, outcome, brute_force = TRUE, order = 3,
                              random_seed, verbose = TRUE, greedy_cutoff = .05) {
  quo_outcome <- rlang::enquo(outcome)

  # Make the gbm model
  formula_str <- paste0(rlang::quo_name(quo_outcome), " ~ .")

  # Get outcome type
  outcome_col <- pull(d, !!rlang::quo_name(quo_outcome))
  n_distinct <- n_distinct(outcome_col)
  if (n_distinct == 2) {
    if (verbose)
      message("Assuming bernoulli from outcome variable")
    if (class(outcome_col) == "factor") {
      d <-
        d %>%
        mutate(!!rlang::quo_name(quo_outcome) := as.numeric(!!quo_outcome) - 1)
    } else if (class(outcome_col) == "logical") {
      d <-
        d %>%
        mutate(!!rlang::quo_name(quo_outcome) := as.numeric(!!quo_outcome))
    }
    distribution <- "bernoulli"
  } else {
    if (class(outcome_col) == "factor") {
      stop("Find interactions doesn't support multiclass outcome variables")
    } else {
      if (verbose)
        message("Assuming gaussian from outcome variable")
      distribution <- "gaussian"
    }
  }


  if (!missing(random_seed))
    set.seed(random_seed)
  m <- gbm(as.formula(formula_str), d, interaction.depth = order,
           distribution = distribution)

  if (brute_force) {
    brute_force_interactions(d, quo_outcome, m, order, verbose, random_seed)
  } else {
    important_features <- rownames(summary(m, plotit = FALSE))[summary(m, plotit = FALSE)$rel.inf != 0]
    interactions <- list(combinations = list(), significance = c())
    starting_order <- 2 # H-statistic starts at 2nd order interactions
    greedy_interactions(d, m, map(important_features, ~.x),
                        important_features, interactions, starting_order, order,
                        greedy_cutoff)
  }
}

#' Estimates the amount of time it will take to calculate all interactions to
#' a specific order
#' @noRd
estimate_time <- function(n_variables, interact_time, order) {
  n_iterations <- 0
  for (i in 2:order) {
    n_iterations <- n_iterations + choose(n_variables, i)
  }
  return(n_iterations * interact_time)
}

#' Calculates the H statistic for evey possible interaction of the order given
#' @noRd
brute_force_interactions <- function(d, quo_outcome, m, order, verbose,
                                     random_seed) {
  # find all the independent variable names
  indepedent_vars <-
    select(d, -!!quo_outcome) %>%
    names()

  if (verbose) {
    begin <- proc.time()
    interact.gbm(m, d, i.var = indepedent_vars[1:2])
    end <- proc.time() - begin

    est <- estimate_time(length(indepedent_vars), end[3], order)
    message("This will take ", est, "seconds")
  }

  # We only want to calculate up to 4th order interactions
  order_limit <-
    if (length(indepedent_vars) > order)
      order
  else
    length(indepedent_vars)

  # fina all combinations from 2nd to 4th order interactions
  combn_vars <- list()
  for (i in 2:order_limit) {
    combn_vars <- c(combn_vars, combn(indepedent_vars, i, simplify = FALSE))
  }

  sig <- map_dbl(combn_vars, ~{
    interact.gbm(m, d, i.var = .x)
  })
  list(combinations = combn_vars, significance = sig)
}

#' Calculates the H statistic for interactions that are related to lower
#' significant interactions. This follows the process of Friedman.
#' @noRd
greedy_interactions <- function(data, gbm_model, testing_interactions,
                                important_features, interactions, cur_order,
                                order_limit, greedy_cutoff) {
  if (cur_order <= order_limit) {
    for (i in 1:length(testing_interactions)) {
      interactions <- greedy_interactions_recursion(data, gbm_model,
                                              testing_interactions[[i]],
                                              important_features,
                                              interactions, cur_order,
                                              order_limit, greedy_cutoff)
    }
  }
  return(interactions)
}

#' Called by greedy_interactions as a helper in recursion
#' @noRd
greedy_interactions_recursion <- function(data, gbm_model, testing_interaction,
                                    important_features, interactions,
                                    cur_order, order_limit, greedy_cutoff) {
  interaction_significance <- NULL
  interaction_combinations <- list()
  interested <- NULL
  for (i in 1:length(important_features)) {
    # get the interaction significance for each combination of variables, i.var
    # finds the h-statistic for interaction significance for all items given
    interaction_combinations[[i]] <- c(testing_interaction,
                                       important_features[i])
    interaction_combinations[[i]] <- interaction_combinations[[i]][
      order(interaction_combinations[[i]])
      ]

    #
    if (important_features[i] %in% testing_interaction) {
      interested[i] <- FALSE
      interaction_significance[i] <- 0
      next
    }

    interaction_significance[i] <-
      gbm::interact.gbm(gbm_model, data,
                        i.var = c(interaction_combinations[[i]]))


    interested[i] <- interaction_significance[i] > greedy_cutoff


    # Remove the interaction if it is reflexive
    if (length(interactions$combinations) > 0)
      if (interested[i] & any(map_lgl(interactions$combinations, ~{
        identical(.x, interaction_combinations[[i]])
      })))
        interested[i] <- FALSE

  }

  # Reorder combinantions to find matches in the future
  interactions$combinations <- c(interactions$combinations,
                                 interaction_combinations[interested])
  interactions$significance <- c(interactions$significance,
                                 interaction_significance[interested])

  # Only continue recursion if there are no more possible interactions. If there
  # is only one, it might find an interaction with itself when we switch to
  # a tree based function.
  if (sum(interested) > 1) {
    interactions <- greedy_interactions(data, gbm_model,
                                     interaction_combinations[interested],
                                     important_features[interested],
                                     interactions, cur_order + 1, order_limit,
                                     greedy_cutoff)
  }

  return(interactions)
}
