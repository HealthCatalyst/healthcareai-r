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

### Tests
test_that("plot.predicted_df stops if there's no outcome", {
  expect_error(plot(dplyr::select(reg_preds_self, -age)), "outcome")
  expect_error(plot(dplyr::select(reg_preds_new, -age)), "outcome")
  expect_error(plot(dplyr::select(class_preds_self, -diabetes)), "outcome")
  expect_error(plot(dplyr::select(class_preds_new, -diabetes)), "outcome")
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
