#' Adds the category count to each category name in a given variable column
#'
#' @description `rename_with_counts` concatenates the count of each category to
#'   its category name given a specific variable. It can be useful in plots and
#'   tables to display the fequency of categories of a variable (see the example
#'   below).
#' @param d a tibble or dataframe
#' @param variable_name the column with counts wanted
#' @return a tibble with the counts appended to the `variable_name` column
#' @export
#'
#' @examples
#' rename_with_counts(pima_diabetes, weight_class)
#'
#' # Below is an example of how `rename_with_counts` can be helpful when
#' # creating plots and tables. This graph shows the outcomes of different
#' # weight classes in `pima_diabetes`. With the added information from
#' # `rename_with_counts`, we can see how common each category is.
#' library(ggplot2)
#' rename_with_counts(pima_diabetes, weight_class) %>%
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
            " designed for numeric columns. This column will be converted to a",
            " character type.")

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
