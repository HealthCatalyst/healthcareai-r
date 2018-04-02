## ----setup, include=FALSE------------------------------------------------
set.seed(43170)
knitr::opts_chunk$set(echo = TRUE, results = "hold", collapse = TRUE, 
                      comment = "# >")
options(tibble.print_min = 5, tibble.print_max = 5)

## ------------------------------------------------------------------------
library(healthcareai)

## ------------------------------------------------------------------------
str(pima_diabetes)

## ------------------------------------------------------------------------
quick_models <- machine_learn(pima_diabetes, patient_id, outcome = diabetes)

## ------------------------------------------------------------------------
quick_models

## ------------------------------------------------------------------------
predictions <- predict(quick_models)
predictions

## ------------------------------------------------------------------------
plot(predictions)

## ------------------------------------------------------------------------
missingness(pima_diabetes)

## ------------------------------------------------------------------------
split_data <- split_train_test(d = pima_diabetes,
                               outcome = diabetes,
                               p = .9,
                               seed = 84105)

## ------------------------------------------------------------------------
prepped_training_data <- prep_data(split_data$train, patient_id, outcome = diabetes,
                                   center = TRUE, scale = TRUE,
                                   collapse_rare_factors = FALSE)

## ------------------------------------------------------------------------
models <- tune_models(d = prepped_training_data,
                      outcome = diabetes,
                      models = "RF",
                      tune_depth = 25,
                      metric = "PR")

## ---- fig.height = 7-----------------------------------------------------
plot(models)

## ------------------------------------------------------------------------
flash_models(d = prepped_training_data,
             outcome = diabetes,
             models = "RF",
             metric = "PR",
             hyperparameters = list(
               RF = list(
                 mtry = 10,
                 splitrule = "extratrees",
                 min.node.size = 20)))

## ------------------------------------------------------------------------
predict(models)

## ------------------------------------------------------------------------
test_predictions <- predict(models, split_data$test)
plot(test_predictions)

## ------------------------------------------------------------------------
regression_models <- machine_learn(pima_diabetes, patient_id, outcome = age)
summary(regression_models)

## ------------------------------------------------------------------------
new_patient <- data.frame(
  pregnancies = 0,
  plasma_glucose = 80,
  diastolic_bp = 55,
  skinfold = 24,
  insulin = NA,
  weight_class = "???",
  pedigree = .2,
  diabetes = "N")
predict(regression_models, new_patient)

