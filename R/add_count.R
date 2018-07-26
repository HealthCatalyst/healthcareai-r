#' Adds the count to a specific variable
#'
#' @description adds the count of each variable to its value
#' @param d a tibble or dataframe
#' @param variable_name the column with counts wanted
#' @return a tibble with the counts appended to the `variable_name` column
#' @export
#'
#' @examples
#' # Adds the count of each animal to its animal listing in the column animal
#' library(tidyverse)
#' d <- tibble(animal = sample(c("cat", "dog", "mouse", "rabbit"), 20, TRUE),
#'   other_var = rnorm(20))
#'
#' add_count(d, animal)
add_count <- function(d, variable_name) {
  if (!is.data.frame(d))
    stop("`d` must be dataframe")

  variable_name <- rlang::enquo(variable_name)

  d <-
    d %>%
    count(!!variable_name) %>%
    left_join(d, ., by = quo_name(variable_name)) %>%
    mutate(!!quo_name(variable_name) := paste0(!!variable_name, " (n = ", n, ")")) %>%
    select(-n)

  return(d)
}
