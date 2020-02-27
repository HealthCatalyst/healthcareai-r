context("explore")

##### Setup
set.seed(400)
m <- machine_learn(pima_diabetes[1:200, ], patient_id, outcome = diabetes,
                   tune = FALSE, n_folds = 2)
multi <- machine_learn(na.omit(pima_diabetes[1:200, ]), patient_id, outcome = weight_class,
                       tune = FALSE, models = "glm")
variabs <-
  attr(m, "recipe")$var_info %>%
  dplyr::filter(role == "predictor") %>%
  split(., .$type) %>%
  purrr::map(dplyr::pull, variable)
sm <- explore(m)
plot_sm <- plot(sm, print = FALSE)

##### explore
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
  expect_warning(g3 <- choose_variables(m["glmnet"], 3, variabs), "glm")
  expect_length(g3, 3)
  expect_true(all(g3 %in% names(pima_diabetes)))
  # No warning if glm and variables were scaled:
  expect_warning({
    pima_diabetes[1:200, ] %>%
      prep_data(patient_id, outcome = diabetes, scale = TRUE) %>%
      flash_models(diabetes, models = "glm", n_folds = 2) %>%
      explore()
  },
  NA)
})

test_that("choose_values nums only", {
  cv <- choose_values(m, vary = c("skinfold", "age"), variables = variabs,
                      numerics = 5, characters = Inf,
                      training_data = pima_diabetes[1:50, ])
  expect_setequal(names(cv), c("skinfold", "age"))
  expect_equal(names(cv[[1]]), names(cv[[2]]))
  expect_true(all(stringr::str_sub(names(cv[[1]]), -1, -1) == "%"))
})

test_that("choose_values noms only", {
  cv <- choose_values(m, vary = "weight_class", variables = variabs, numerics = 5,
                      characters = Inf, training_data = pima_diabetes[1:50, ])
  expect_equal(names(cv), "weight_class")
  expect_setequal(cv$weight_class, unique(pima_diabetes$weight_class[1:50]))
})

test_that("choose_values both", {
  cv <- choose_values(m, vary = c("weight_class", "age"), variables = variabs,
                      numerics = 5, characters = Inf,
                      training_data = pima_diabetes[1:50, ])
  expect_setequal(names(cv), c("weight_class", "age"))
  expect_true(is.numeric(cv$age))
  expect_true(is.character(cv$weight_class))
})

test_that("choose_values limit characters", {
  cv <- choose_values(m, vary = c("weight_class", "age"), variables = variabs, numerics = 5,
                      characters = 3, training_data = pima_diabetes[1:50, ])
  pima_diabetes %>%
    dplyr::count(weight_class) %>%
    dplyr::top_n(3, n) %>%
    pull(weight_class) %>%
    expect_setequal(cv$weight_class, .)
})

test_that("choose_values numerics is integer", {
  cv <- choose_values(m, vary = c("weight_class", "age"), variables = variabs,
                      numerics = 3, characters = Inf, training_data = pima_diabetes[1:50, ])
  expect_setequal(names(cv), c("weight_class", "age"))
  expect_length(cv$age, 3)
  expect_setequal(names(cv$age), c("5%", "50%", "95%"))
})

test_that("choose_values numerics is quantiles", {
  cv <- choose_values(m, vary = "plasma_glucose", variables = variabs,
                      numerics = c(.33, .67), characters = Inf,
                      training_data = pima_diabetes[1:50, ])
  expect_equal(names(cv), "plasma_glucose")
  expect_length(cv$plasma_glucose, 2)
  expect_setequal(names(cv$plasma_glucose), c("33%", "67%"))
})

test_that("choose_static_values nums only", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = character(), numeric = c("insulin", "age")),
                             hold = list(numerics = median, characters = Mode),
                             training_data = pima_diabetes[1:50, ])
  expect_true(is.list(sv))
  expect_setequal(names(sv), c("insulin", "age"))
  purrr::walk(sv, expect_length, 1)
})

test_that("choose_static_values noms only", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = "weight_class", numeric = character()),
                             hold = list(numerics = median, characters = Mode),
                             training_data = pima_diabetes[1:50, ])
  expect_true(is.list(sv))
  expect_setequal(names(sv), "weight_class")
  purrr::walk(sv, expect_length, 1)
})

test_that("choose_static_values both", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = "weight_class", numeric = c("insulin", "age")),
                             hold = list(numerics = median, characters = Mode),
                             training_data = pima_diabetes[1:50, ])
  expect_true(is.list(sv))
  expect_setequal(names(sv), c("insulin", "age", "weight_class"))
  purrr::walk(sv, expect_length, 1)
})

test_that("choose_static_values hold from training data", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = "weight_class", numeric = c("insulin", "age")),
                             hold = pima_diabetes[3, ],
                             training_data = pima_diabetes[1:50, ])
  expect_true(is.list(sv))
  expect_setequal(names(sv), c("insulin", "age", "weight_class"))
  purrr::walk(sv, expect_length, 1)
})

test_that("choose_static_values hold is custom values", {
  sv <- choose_static_values(m,
                             static_variables = list(nominal = "weight_class", numeric = c("insulin", "age")),
                             hold = list(pregnancies = 0, plasma_glucose = 99, weight_class = "obese",
                                         insulin = 3, age = 32),
                             training_data = pima_diabetes[1:50, ])
  expect_true(is.list(sv))
  expect_setequal(names(sv), c("insulin", "age", "weight_class"))
  purrr::walk(sv, expect_length, 1)
})

test_that("explore returns a tibble with custom class", {
  expect_s3_class(sm, "tbl_df")
  expect_s3_class(sm, "explore_df")
})

test_that("explore can use any algorithm", {
  suppressWarnings({
    purrr::map_lgl(seq_along(m), ~ is.tbl(explore(m[.x]))) %>%
      all() %>%
      expect_true()
  })
})

test_that("vary varies the correct number of variables", {
  explore(m, vary = 1) %>%
    dplyr::select(-predicted_diabetes) %>%
    purrr::map_lgl(~ dplyr::n_distinct(.x) > 1) %>%
    sum() %>%
    expect_equal(1)
  explore(m, vary = 5) %>%
    dplyr::select(-predicted_diabetes) %>%
    purrr::map_lgl(~ dplyr::n_distinct(.x) > 1) %>%
    sum() %>%
    expect_equal(5)
})

test_that("list of values to vary works right", {
  values_to_use <- list(
    plasma_glucose = c(50, 100, 150),
    weight_class = c("underweight", "normal", NA)
  )
  preds <- explore(m, vary = values_to_use)
  expect_s3_class(preds, "explore_df")
  expect_setequal(preds$plasma_glucose, values_to_use$plasma_glucose)
  expect_setequal(preds$weight_class, values_to_use$weight_class)
})

test_that("explore hold custom functions (mean instead of median for numerics)", {
  sm_def <- explore(m, vary = c("weight_class", "skinfold"))
  sm2 <- explore(m, vary = c("weight_class", "skinfold"),
                 hold = list(numerics = mean, characters = Mode))
  same <- purrr::map2(sm_def, sm2, ~ isTRUE(all.equal(.x, .y)))
  expect_true(same$weight_class)
  expect_false(same$plasma_glucose)
  expect_false(same$predicted_diabetes)
})

test_that("explore hold row from test data", {
  p51 <- explore(m, hold = pima_diabetes[51, ], vary = c("weight_class"))
  varying <- purrr::map_lgl(p51, ~ dplyr::n_distinct(.x) > 1)
  expect_setequal(names(varying)[varying], c("predicted_diabetes", "weight_class"))
  p51[, !varying] %>%
    dplyr::distinct() %>%
    expect_equal(pima_diabetes[51, which(names(pima_diabetes) %in% names(varying)[!varying])])
})

test_that("explore hold custom list", {
  ch <- explore(m,
                vary = dplyr::setdiff(names(pima_diabetes), c("patient_id", "diabetes", "age", "skinfold")),
                hold = list(age = 21, skinfold = 18))
  varying <- purrr::map_lgl(ch, ~ dplyr::n_distinct(.x) > 1)
  expect_setequal(names(varying)[!varying], c("age", "skinfold"))
  expect_equal(unique(ch$age), 21)
  expect_equal(unique(ch$skinfold), 18)
})

test_that("explore returns right number of character values", {
  expect_equal(dplyr::n_distinct(explore(m, vary = "weight_class", characters = 2)$weight_class), 2)
  expect_equal(dplyr::n_distinct(explore(m, characters = 4)$weight_class), 4)
  expect_equal(dplyr::n_distinct(sm$weight_class), dplyr::n_distinct(pima_diabetes$weight_class[1:50]))
})

test_that("explore returns the right number of numeric values", {
  expect_equal(dplyr::n_distinct(explore(m, vary = "plasma_glucose", numerics = 2)$plasma_glucose), 2)
  expect_equal(dplyr::n_distinct(explore(m, numerics = 9)$plasma_glucose), 9)
  expect_equal(dplyr::n_distinct(explore(m, numerics = c(.1, .3, .8))$plasma_glucose), 3)
})

test_that("explore errors as expected", {
  expect_error(explore(pima_diabetes), "model_list")
  expect_error(explore(structure(m, recipe = NULL)), "prep_data")
  expect_error(explore(m, vary = c("age", "not_a_var")), "not_a_var")
  expect_error(explore(m, hold = list(numerics = mean)), "hold")
  expect_error(explore(m, hold = list(characters = Mode)), "hold")
  expect_error(explore(m, hold = list(characters = letters)), "hold")
  expect_error(explore(m, hold = list(numerics = mean, characters = Mode)), NA)
  expect_error(explore(m, hold = list(median, Mode)), "named")
  expect_error(explore(m, hold = list(age = 50)), "hold")
})

##### explore generics
test_that("printing a explored df doesn't print training performance info", {
  sim_print <- capture_output(sim_mess <- capture_messages(print(sm)))
  sim_output <- paste(sim_print, sim_mess)
  expect_false(stringr::str_detect(sim_print, "Performance"))
})

test_that("plot.explore_df is registered", {
  stringr::str_detect(methods("plot"), "explore_df") %>%
    any() %>%
    expect_true()
})

test_that("plot.explore_df returns a ggplot", {
  expect_s3_class(plot_sm, "gg")
})

test_that("plot.cf args work", {
  default_plot <- plot(sm, print = FALSE, jitter_y = FALSE)
  suppressWarnings({
    expect_false(isTRUE(all.equal(
      plot(sm, print = FALSE, jitter_y = FALSE, n_use = 2),
      plot(sm, print = FALSE, jitter_y = FALSE, n_use = 2, aggregate_fun = mean))))
  })
  expect_message(plot(sm, print = FALSE, n_use = 2), "aggregate")

  # make sure each argument changes something
  altered_plots <- list(
    plot(sm, print = FALSE, jitter_y = FALSE, reorder_categories = FALSE),
    plot(sm, print = FALSE, jitter_y = TRUE),
    plot(sm, print = FALSE, jitter_y = FALSE, x_var = weight_class),
    plot(sm, print = FALSE, jitter_y = FALSE, color_var = weight_class),
    plot(sm, print = FALSE, jitter_y = FALSE, font_size = 8),
    plot(sm, print = FALSE, jitter_y = FALSE, strip_font_size = .5),
    plot(sm, print = FALSE, jitter_y = FALSE, line_width = 1),
    plot(sm, print = FALSE, jitter_y = FALSE, line_alpha = .5),
    plot(sm, print = FALSE, jitter_y = FALSE, rotate_x = TRUE)
  )
  purrr::map_lgl(altered_plots, ~ isTRUE(all.equal(.x, default_plot))) %>%
    any() %>%
    expect_false()
  use2 <- plot(sm, print = FALSE, jitter_y = FALSE, n_use = 2)
  expect_equal(nrow(ggplot2::ggplot_build(use2)$layout$layout), 1)
  use1 <- plot(sm, print = FALSE, jitter_y = FALSE, n_use = 1)
  expect_false(isTRUE(all.equal(use1, use2)))
  use3 <- plot(sm, print = FALSE, jitter_y = FALSE, n_use = 3)
  expect_false(isTRUE(all.equal(use3, use2)))
})

#### plot.explore_df helpers
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

test_that("map_variables errors informatively", {
  expect_error(map_variables(vars, rlang::quo("something else"), rlang::quo()), "x_var")
  expect_error(map_variables(vars, rlang::quo(), rlang::quo("something else")), "color_var")
})

test_that("n_use > 4 errors informatively", {
  expect_error(plot(sm, print = FALSE, n_use = 5), "n_use")
})

test_that("explore can handle once-logical features", {
  n <- 20
  d <- tibble::tibble(x = sample(c(FALSE, TRUE), n, TRUE),
                      other = rnorm(n),
                      another = sample(c("dog", "cat"), n, TRUE),
                      y = sample(c("Y", "N"), n, TRUE))
  m <- machine_learn(d, outcome = y, tune = FALSE, models = "xgb", n_folds = 2)
  expect_error(expl <- explore(m), NA)
  expect_s3_class(expl, "explore_df")
})

test_that("explore uses scaled features appropriately", {
  n <- 100
  d <- data.frame(x1 = sort(rnorm(n, 10, 100)),
                  x2 = sort(rpois(n, 5)),
                  x3 = runif(n, -10, 10),
                  x4 = sample(letters, n, TRUE),
                  y = sort(rnorm(n)))
  # with centering and scaling
  pd <- prep_data(d, outcome = y, center = TRUE, scale = TRUE)
  m <- flash_models(pd, outcome = y, models = "xgb", n_folds = 3)
  var1 <-
    explore(m) %>%
    dplyr::pull(predicted_y) %>%
    stats::var()

  # without centering and scaling
  pd2 <- prep_data(d, outcome = y)
  m2 <- flash_models(pd2, outcome = y, models = "xgb", n_folds = 3)
  var2 <-
    explore(m2) %>%
    dplyr::pull(predicted_y) %>%
    stats::var()
  # var2 was 100x greater before being fixed, so large tolerance here is okay
  expect_equal(var1, var2, tolerance = min(var1, var2))
})

test_that("multiclass errors", {
  expect_error(explore(multi), "multiclass")
})

test_that("test removed data throws error", {
  save_models(m)
  reloaded_m <- load_models("models.RDS")
  expect_error(explore(reloaded_m), "Explore requires that data is ")
  file.remove("models.RDS")
})
