## Notes on programming with `dplyr`/`rlang`

This is all for letting users provided unquoted variable names, which let's us program with tidyverse tools, but we have to adopt a few workarounds. Here is the [official vignette](https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html); this is meant to be a quick reference guide. See also `pivot.R` for examples in action.

Imagine a user has called a function like this: `fun(d = df, var = my_special_variable_name)`

At the beginning of a function you have to capture user-provided variables before they are evaluted. Do this with `caputred_var <- rlang::enquo(var)`. That creates a "quosure". Normally you'd probably just use `var <- rlang::enquo(var)`, but this should be clearer.

- To convert a quosure to string, e.g. to get "my_special_variable_name" use `rlang::quo_name(captured_var)`

Normal tidyverse functions require some special syntax to use quosures:

- To extract a vector from DF: `pull(d, !!captured_var)`

- To filter on a user-supplied column `filter(d, (!!captured_var) == 0)`

- To create a new column with a user-provided name: `mutate(d, !!quo_name(captured_var) := 1:nrow(df))`

Putting some concepts together. From a call like `fun(d, var1, var2, var3)`

```
d %>%
group_by(!!var1, !!var2) %>%
summarize(!!quo_name(var3) := some_function(!!var3))
```

If you pass quosures between functions, they arrive at the second function as quosures, obviously. To test those functions (or, more generally, call them directly outside of the normal function context) you can specify the arguments as `rlang::quo(var)`. For example:

```
fun1 <- function(d, var) {
  var <- rlang::enquo(var)
  fun2(d, var)
}

fun2 <- function(d, var) 
  sum(dplyr::pull(d, !!var))

d <- data.frame(x = 1:3)
fun1(d, x)
fun2(d, rlang::quo(x))
```
