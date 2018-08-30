context("Checking prediction plots")

### Setup
d_reg <- prep_data(pima_diabetes[1:100, ], patient_id, outcome = age)
m_reg <- tune_models(d_reg, outcome = age, n_folds = 2, tune_depth = 1, models = "rf")
reg_preds_self <- predict(m_reg)
reg_preds_new <- predict(m_reg, pima_diabetes[101:110, ])

d_class <- prep_data(pima_diabetes[1:100, ], patient_id, outcome = diabetes)
m_class <- tune_models(d_class, outcome = diabetes, n_folds = 2, tune_depth = 1, models = "rf")
class_preds_self <- predict(m_class)
class_preds_new <- predict(m_class, pima_diabetes[101:110, ])

d_multi <- prep_data(iris[1:140, ], outcome = Species)
m_multi <- tune_models(d_multi, outcome = Species, n_folds = 2, tune_depth = 1, models = "rf")
multi_preds_self <- predict(m_multi)
multi_preds_new <- predict(m_multi, iris[141:150, ])
multi_preds_single <- predict(m_multi, iris[100, ])

### Tests
test_that("plot.predicted_df stops if there's no outcome", {
  expect_error(plot(dplyr::select(reg_preds_self, -age)), "outcome")
  expect_error(plot(dplyr::select(reg_preds_new, -age)), "outcome")
  expect_error(plot(dplyr::select(class_preds_self, -diabetes)), "outcome")
  expect_error(plot(dplyr::select(class_preds_new, -diabetes)), "outcome")
  expect_error(plot(dplyr::select(multi_preds_self, -Species)), "outcome")
  expect_error(plot(dplyr::select(multi_preds_new, -Species)), "outcome")
})

test_that("plot.predicted_df stops if outcome vector is wrong length or class", {
  expect_error(plot(dplyr::select(reg_preds_self, -age),
                    outcomes = 1:5),
               "length")
  expect_error(plot(dplyr::select(reg_preds_self, -age),
                    outcomes = sample(letters, nrow(reg_preds_self), TRUE)),
               "class")
  expect_error(plot(dplyr::select(class_preds_self, -diabetes),
                    outcomes = sample(unique(d_class$diabetes), 5, TRUE)),
               "length")
  expect_error(plot(dplyr::select(class_preds_self, -diabetes),
                    outcomes = sample(10, nrow(class_preds_self), TRUE)),
               "class")
  expect_error(plot(dplyr::select(multi_preds_self, -Species),
                    outcomes = sample(unique(d_multi$Species), 5, TRUE)),
               "length")
  expect_error(plot(dplyr::select(multi_preds_self, -Species),
                    outcomes = sample(10, nrow(multi_preds_self), TRUE)),
               "same data")
})

test_that("plot.predicted_df warns but works if outcomes present in df and passed in", {
  expect_warning(p <- plot(reg_preds_new, outcomes = 1:10, print = FALSE), "outcome")
  expect_s3_class(p, "gg")
  expect_warning(p <- plot(class_preds_new,
                           outcomes = sample(unique(d_class$diabetes), nrow(class_preds_new), TRUE),
                           print = FALSE),
                 "outcome")
  expect_s3_class(p, "gg")
})

test_that("plot_regression_predictions handles defaults", {
  expect_s3_class(plot(reg_preds_new, print = FALSE), "gg")
  expect_s3_class(plot(reg_preds_self, print = FALSE), "gg")
})

test_that("plot_regression_predictions handles separately supplied outcomes", {
  expect_s3_class(plot(dplyr::select(reg_preds_self, -age),
                       outcomes = reg_preds_self$age, print = FALSE),
                  "gg")
})

test_that("plot_classification_predictions handles defaults", {
  expect_s3_class(plot(class_preds_new, print = FALSE), "gg")
  expect_s3_class(plot(class_preds_self, print = FALSE), "gg")
})

test_that("plot_classification_predictions handles separately supplied outcomes", {
  expect_s3_class(plot(dplyr::select(class_preds_self, -diabetes),
                       outcomes = class_preds_self$diabetes,
                       print = FALSE),
                  "gg")
})

test_that("plot_multiclass_predictions handles defaults", {
  expect_s3_class(plot(multi_preds_new, print = FALSE), "gg")
  expect_s3_class(plot(multi_preds_self, print = FALSE), "gg")
})

test_that("plot_multiclass_predictions handles separately supplied outcomes", {
  expect_s3_class(plot(dplyr::select(multi_preds_self, -Species),
                       outcomes = multi_preds_self$Species, print = FALSE),
                  "gg")
})

test_that("single-class multiclass are correct dimensions", {
  p <- plot(multi_preds_single, print = FALSE)
  expect_equal(length(unique(p$data$Species)), 1)
  expect_equal(length(unique(p$data$predicted_Species)), 3)
})

test_that("multiclass correct predictions are on main diagonal", {
  p <- plot(multi_preds_self, print = FALSE)
  expect_true(all(levels(p$data$Species) == rev(levels(p$data$predicted_Species))))
})

test_that("Arguments to plot.predicted_df get passed to plot_regression_predictions", {
  my_title <- "this is my title"
  p <- plot(reg_preds_self, title = my_title, point_size = 2,
             point_alpha = .5, font_size = 18, print = FALSE)
  expect_s3_class(p, "gg")
  expect_equal(ggplot_build(p)$plot$labels$title, my_title)
})

test_that("Arguments to plot.predicted_df get passed to plot_classification_predictions", {
  my_title <- "this is my title"
  p <- plot(class_preds_self,
            title = my_title,
            fill_colors = c(Y = "green", N = "red"),
            fill_alpha = .2,
            curve_flex = .3,
            font_size = 18,
            print = FALSE)
  expect_s3_class(p, "gg")
  expect_equal(ggplot_build(p)$plot$labels$title, my_title)
})

test_that("Arguments to plot.predicted_df get passed to plot_multiclass_predictions", {
  my_title <- "this is my title"
  p <- plot(multi_preds_self,
            title = my_title,
            conf_colors = c(Y = "green", N = "red"),
            print = FALSE)
  expect_s3_class(p, "gg")
  expect_equal(ggplot_build(p)$plot$labels$title, my_title)
  expect_equal(ggplot_build(p)$data[[1]]$fill[1], "#FF0000") # green
})

test_that("fixed_aspect works", {
  reg_fix <- plot(reg_preds_self, print = FALSE)
  reg_unfix <- plot(reg_preds_self, fixed_aspect = FALSE, print = FALSE)
  expect_false(isTRUE(all.equal(reg_fix, reg_unfix)))
  class_fix <- plot(class_preds_new, fixed_aspect = TRUE, print = FALSE)
  class_unfix <- plot(class_preds_new, print = FALSE)
  class_unfix_force <- plot(class_preds_new, fixed_aspect = FALSE, print = FALSE)
  expect_false(isTRUE(all.equal(class_fix, class_unfix)))
  expect_equivalent(class_unfix, class_unfix_force)
})

test_that("lines and labels are added when outcome_groups present", {
  w_groups <-
    predict(m_class, outcome_groups = 1 / 3) %>%
    plot(print = FALSE) %>%
    ggplot_build()
  wo_groups <-
    predict(m_class) %>%
    plot(print = FALSE) %>%
    ggplot_build()
  expect_length(w_groups$data, 3)
  expect_length(wo_groups$data, 1)
  expect_true("linetype" %in% names(w_groups$data[[2]]))
  expect_equal(nrow(w_groups$data[[3]]), 2)  # one for each label
})

test_that("lines and labels are added when risk_groups present", {
  groups <- c("low", "mid", "high")
  w_groups <-
    predict(m_class, risk_groups = groups) %>%
    plot(print = FALSE) %>%
    ggplot_build()
  wo_groups <-
    predict(m_class) %>%
    plot(print = FALSE) %>%
    ggplot_build()
  expect_length(w_groups$data, 3)
  expect_length(wo_groups$data, 1)
  expect_equal(nrow(w_groups$data[[2]]), 2)
  expect_setequal(groups, w_groups$data[[3]]$label)
})
