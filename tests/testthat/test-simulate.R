context("Simulate")

set.seed(574)
m <- machine_learn(pima_diabetes[1:50, ], patient_id, outcome = diabetes,
                   tune = FALSE, n_folds = 2)

test_that("simulate returns a data frame", {
  expect_s3_class(simulate(m), "tbl_df")
})

test_that("simulate can use any algorithm", {
  purrr::map_lgl(seq_along(m), ~ expect_s3_class(simulate(m[.x]), "tbl_df"))
})

test_that("vary varies the correct number of variables", {
  simulate(m, vary = 1) %>%
    dplyr::select(v1, -patient_id) %>%
    purrr::map_lgl(~ n_distinct(.x) > 1) %>%
    sum() %>%
    expect_equal(1)
  simulate(m, vary = 5) %>%
    dplyr::select(v1, -patient_id) %>%
    purrr::map_lgl(~ n_distinct(.x) > 1) %>%
    sum() %>%
    expect_equal(5)
})
