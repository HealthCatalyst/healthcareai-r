context("Checking evaluate")

# Setup
d <- na.omit(pima_diabetes)[1:100, ]
dtest <- na.omit(pima_diabetes)[101:110, ]

r_models <- machine_learn(d, patient_id, outcome = plasma_glucose, tune = FALSE, n_folds = 2)
c_models <- machine_learn(d, patient_id, outcome = diabetes, tune = FALSE, n_folds = 2)

r_preds_training <- predict(r_models)
c_preds_training <- predict(c_models)
r_preds_test <- predict(r_models, dtest)
c_preds_test <- predict(c_models, dtest)
