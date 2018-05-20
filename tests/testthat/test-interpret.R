context("Checking interpret")

m <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = diabetes)

test_that("interpret returns a tbl iff glmnet is in model_list", {
  expect_s3_class(interpret(m), "tbl_df")
  expect_s3_class(interpret(m["glmnet"]), "tbl_df")
})

test_that("interpret errors with ref to var-imp if no glmnet present", {
  expect_error(interpret(m["Random Forest"]), "get_variable_importance")
  expect_error(interpret(m[which(names(m) != "glmnet")]), "get_variable_importance")
})
