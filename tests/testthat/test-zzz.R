context("Teardown")

test_that("Remove files written during testing and confirm they are removed.", {
    # Remove log files, model data, and plot output
    to_remove <- list.files(pattern = "(txt$)|(rda$)|(pdf$)|(png$)")
    file.remove(to_remove)
    expect_true(all(!file.exists(to_remove)))
})
