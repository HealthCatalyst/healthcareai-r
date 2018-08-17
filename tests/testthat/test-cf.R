context("predict_counterfactual")

##### Setup
set.seed(574)
m <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = diabetes,
                   tune = FALSE, n_folds = 2)
variabs <-
  attr(m, "recipe")$var_info %>%
  dplyr::filter(role == "predictor") %>%
  split(., .$type) %>%
  purrr::map(dplyr::pull, variable)
sm <- predict_counterfactual(m)
plot_sm <- plot(sm)

##### predict_counterfactual
test_that("test_presence", {
  expect_error(test_presence(c("weight_class", "insulin", "age"), variabs), NA)
  against <- list(a = c("one", "thing", "or"), b = "another")
  expect_error(test_presence(c("one", "another"), against), NA)
  expect_error(test_presence("one", against), NA)
  expect_error(test_presence("another", against), NA)
  expect_error(test_presence(c("one", "thing"), against), NA)
  expect_error(test_presence("a", against), "aren't predictors")
})

test_that("choose_variables default", {
  cv <- list(
    c1 = choose_variables(m, vary = 1, variables = variabs),
    c5 = choose_variables(m, vary = 5, variables = variabs),
    c99 = choose_variables(m, vary = 99, variables = variabs)
  )
  purrr::map_lgl(cv, ~ all(.x %in% names(pima_diabetes))) %>%
    all() %>%
    expect_true()
  expect_length(cv$c1, 1)
  expect_length(cv$c5, 5)
  expect_length(cv$c99, length(unlist(variabs)))
})

test_that("choose_variables glm", {
  g3 <- choose_variables(m["glmnet"], 3, variabs)
  expect_length(g3, 3)
  expect_true(all(g3 %in% names(pima_diabetes)))
})

test_that("choose_values nums only", {
  cv <- choose_values(m, vary = c("skinfold", "age"), variables = variabs, numerics = 5, characters = Inf)
  expect_setequal(names(cv), c("skinfold", "age"))
  purrr::map_lgl(cv, ~ dplyr::setequal(names(.x), c("0%", "25%", "50%", "75%", "100%"))) %>%
    all() %>%
    expect_true()
})

test_that("choose_values noms only", {
  cv <- choose_values(m, vary = "weight_class", variables = variabs, numerics = 5, characters = Inf)
  expect_equal(names(cv), "weight_class")
  expect_setequal(cv$weight_class, unique(pima_diabetes$weight_class[1:50]))
})

test_that("choose_values both", {
  cv <- choose_values(m, vary = c("weight_class", "age"), variables = variabs, numerics = 5, characters = Inf)
  expect_setequal(names(cv), c("weight_class", "age"))
  expect_true(is.numeric(cv$age))
  expect_true(is.character(cv$weight_class))
})

test_that("choose_values limit characters", {
  cv <- choose_values(m, vary = c("weight_class", "age"), variables = variabs, numerics = 5, characters = 3)
  pima_diabetes %>%
    dplyr::count(weight_class) %>%
    dplyr::top_n(3, n) %>%
    pull(weight_class) %>%
    expect_setequal(cv$weight_class, .)
})

test_that("choose_values numerics is integer", {
  cv <- choose_values(m, vary = c("weight_class", "age"), variables = variabs, numerics = 3, characters = Inf)
  expect_setequal(names(cv), c("weight_class", "age"))
  expect_length(cv$age, 3)
  expect_setequal(names(cv$age), c("0%", "50%", "100%"))
})

test_that("choose_values numerics is quantiles", {
  cv <- choose_values(m, vary = "plasma_glucose", variables = variabs, numerics = c(.33, .67), characters = Inf)
  expect_equal(names(cv), "plasma_glucose")
  expect_length(cv$plasma_glucose, 2)
  expect_setequal(names(cv$plasma_glucose), c("33%", "67%"))
})

test_that("choose_static_values nums only", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = character(), numeric = c("insulin", "age")),
                             hold = list(numerics = median, characters = Mode))
  expect_true(is.list(sv))
  expect_setequal(names(sv), c("insulin", "age"))
  purrr::walk(sv, expect_length, 1)
})

test_that("choose_static_values noms only", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = "weight_class", numeric = character()),
                             hold = list(numerics = median, characters = Mode))
  expect_true(is.list(sv))
  expect_setequal(names(sv), "weight_class")
  purrr::walk(sv, expect_length, 1)
})

test_that("choose_static_values both", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = "weight_class", numeric = c("insulin", "age")),
                             hold = list(numerics = median, characters = Mode))
  expect_true(is.list(sv))
  expect_setequal(names(sv), c("insulin", "age", "weight_class"))
  purrr::walk(sv, expect_length, 1)
})

test_that("choose_static_values hold from training data", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = "weight_class", numeric = c("insulin", "age")),
                             hold = pima_diabetes[3, ])
  expect_true(is.list(sv))
  expect_setequal(names(sv), c("insulin", "age", "weight_class"))
  purrr::walk(sv, expect_length, 1)
})

test_that("choose_static_values hold is custom values", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = "weight_class", numeric = c("insulin", "age")),
                             hold = list(pregnancies = 0, plasma_glucose = 99, weight_class = "obese",
                                         insulin = 3, age = 32))
  expect_true(is.list(sv))
  expect_setequal(names(sv), c("insulin", "age", "weight_class"))
  purrr::walk(sv, expect_length, 1)
})

test_that("predict_counterfactual returns a tibble with custom class", {
  expect_s3_class(sm, "tbl_df")
  expect_s3_class(sm, "cf_df")
})

test_that("predict_counterfactual can use any algorithm", {
  purrr::map_lgl(seq_along(m), ~ is.tbl(predict_counterfactual(m[.x]))) %>%
    all() %>%
    expect_true()
})

test_that("vary varies the correct number of variables", {
  predict_counterfactual(m, vary = 1) %>%
    dplyr::select(-predicted_diabetes) %>%
    purrr::map_lgl(~ dplyr::n_distinct(.x) > 1) %>%
    sum() %>%
    expect_equal(1)
  predict_counterfactual(m, vary = 5) %>%
    dplyr::select(-predicted_diabetes) %>%
    purrr::map_lgl(~ dplyr::n_distinct(.x) > 1) %>%
    sum() %>%
    expect_equal(5)
})

test_that("predict_counterfactual hold custom functions (mean instead of median for numerics)", {
  sm_def <- predict_counterfactual(m, vary = c("weight_class", "skinfold"))
  sm2 <- predict_counterfactual(m, vary = c("weight_class", "skinfold"),
                  hold = list(numerics = mean, characters = Mode))
  same <- purrr::map2(sm_def, sm2, ~ isTRUE(all.equal(.x, .y)))
  expect_true(same$weight_class)
  expect_false(same$plasma_glucose)
  expect_false(same$predicted_diabetes)
})

test_that("predict_counterfactual hold row from test data", {
  p51 <- predict_counterfactual(m, hold = pima_diabetes[51, ], vary = c("weight_class"))
  varying <- purrr::map_lgl(p51, ~ dplyr::n_distinct(.x) > 1)
  expect_setequal(names(varying)[varying], c("predicted_diabetes", "weight_class"))
  p51[, !varying] %>%
    dplyr::distinct() %>%
    expect_equal(pima_diabetes[51, which(names(pima_diabetes) %in% names(varying)[!varying])])
})

test_that("predict_counterfactual hold custom list", {
  ch <- predict_counterfactual(m,
                  vary = dplyr::setdiff(names(pima_diabetes), c("patient_id", "diabetes", "age", "skinfold")),
                  hold = list(age = 21, skinfold = 18))
  varying <- purrr::map_lgl(ch, ~ dplyr::n_distinct(.x) > 1)
  expect_setequal(names(varying)[!varying], c("age", "skinfold"))
  expect_equal(unique(ch$age), 21)
  expect_equal(unique(ch$skinfold), 18)
})

test_that("predict_counterfactual returns right number of character values", {
  expect_equal(dplyr::n_distinct(predict_counterfactual(m, vary = "weight_class", characters = 2)$weight_class), 2)
  expect_equal(dplyr::n_distinct(predict_counterfactual(m, characters = 4)$weight_class), 4)
  expect_equal(dplyr::n_distinct(sm$weight_class), dplyr::n_distinct(pima_diabetes$weight_class[1:50]))
})

test_that("predict_counterfactual returns the right number of numeric values", {
  expect_equal(dplyr::n_distinct(predict_counterfactual(m, vary = "plasma_glucose", numerics = 2)$plasma_glucose), 2)
  expect_equal(dplyr::n_distinct(predict_counterfactual(m, numerics = 9)$plasma_glucose), 9)
  expect_equal(dplyr::n_distinct(predict_counterfactual(m, numerics = c(.1, .3, .8))$plasma_glucose), 3)
})

##### predict_counterfactual generics
test_that("printing a predict_counterfactuald df doesn't print training performance info", {
  sim_print <- capture_output( sim_mess <- capture_messages( print(sm)))
  sim_output <- paste(sim_print, sim_mess)
  expect_false(stringr::str_detect(sim_print, "Performance"))
})

test_that("plot.cf_df is registered", {
  stringr::str_detect(methods("plot"), "cf_df") %>%
    any() %>%
    expect_true()
})

test_that("plot.cf_df returns a ggplot", {
  expect_s3_class(plot_sm, "gg")
})

#### plot.cf_df helpers
vars <- tibble::tibble(variable = letters[1:4],
                       numeric = c(FALSE, TRUE, TRUE, FALSE),
                       nlev = 5)

test_that("map_variables without mappings specified", {
  # First var nominal
  map_variables(vars, x_var = rlang::quo(), color_var = rlang::quo()) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("color", "x", "facet", "facet"))
  # First var numeric
  vars[c(2, 1, 3, 4), ] %>%
    map_variables(x_var = rlang::quo(), color_var = rlang::quo()) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("x", "color", "facet", "facet"))
  # Only one variable
  map_variables(vars[1, ], x_var = rlang::quo(), color_var = rlang::quo()) %>%
    dplyr::pull(map_to) %>%
    expect_equal("x")
  map_variables(vars[2, ], x_var = rlang::quo(), color_var = rlang::quo()) %>%
    dplyr::pull(map_to) %>%
    expect_equal("x")
  # Only numerics
  vars %>%
    dplyr::filter(numeric) %>%
    map_variables(x_var = rlang::quo(), color_var = rlang::quo()) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("x", "color"))
  # Only nominals
  vars %>%
    dplyr::filter(!numeric) %>%
    map_variables(x_var = rlang::quo(), color_var = rlang::quo()) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("x", "color"))
})

test_that("map_variables with mappings specified", {
  vars %>%
    map_variables(x_var = rlang::quo(a), color_var = rlang::quo()) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("x", "color", "facet", "facet"))
  vars %>%
    map_variables(x_var = rlang::quo(d), color_var = rlang::quo()) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("color", "facet", "facet", "x"))
  vars %>%
    map_variables(x_var = rlang::quo(), color_var = rlang::quo(b)) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("facet", "color", "x", "facet"))
  vars %>%
    map_variables(x_var = rlang::quo(d), color_var = rlang::quo(b)) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("facet", "color", "facet", "x"))
  vars %>%
    dplyr::filter(!numeric) %>%
    map_variables(x_var = rlang::quo(), color_var = rlang::quo(a)) %>%
    dplyr::pull(map_to) %>%
    expect_equal(c("color", "x"))
  expect_error(map_variables(vars[1, ], color_var = rlang::quo(a)), "color")
})
