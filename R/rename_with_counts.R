#' Concatenates the count to the end specific variable value
#'
#' @description It can be useful in plots and tables to display the fequency of
#'   categories. `rename_with_counts` concatenates the count of each variable
#'   to its variable value to make these plots and tables.
#' @param d a tibble or dataframe
#' @param variable_name the column with counts wanted
#' @return a tibble with the counts appended to the `variable_name` column
#' @export
#'
#' @examples
#' library(ggplot2)
#' healthcareai::rename_with_counts(pima_diabetes, weight_class) %>%
#'   ggplot(aes(x = reorder(weight_class, diabetes, function(x) mean(x == "Y")),
#'              fill = diabetes)) +
#'   geom_bar(position = "fill") +
#'   coord_flip()
#'
rename_with_counts <- function(d, variable_name) {
  if (!is.data.frame(d))
    stop("`d` must be dataframe")

  variable_name <- rlang::enquo(variable_name)

  if (is.numeric(d %>% pull(!!variable_name)))
    warning("`variable_name` is an numeric column. `rename_with_counts` is not",
            " designed for numeric columns")

  d <-
    d %>%
    count(!!variable_name) %>%
    left_join(d, ., by = quo_name(variable_name)) %>%
    mutate(
      !!quo_name(variable_name) := paste0(!!variable_name, " (n = ", n, ")")
    ) %>%
    select(-n)

  return(d)
}
