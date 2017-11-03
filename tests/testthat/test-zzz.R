context("Teardown")

test_that("Remove files written during testing and confirm they are removed.", {
  
    # Precautionary check that we're not deleting files anywhere but testthat
    wd <- getwd()
    path <- strsplit(wd, "/|\\\\")[[1]]
    path_right <- all.equal(c("healthcareai-r", "tests", "testthat"), 
                            path[length(path) - 2:0])
    if (!path_right)
      stop("testing happening in the wrong directory. Test via devtools::test")
  
    # Remove log files, model data, and plot output
    to_remove <- list.files(pattern = "(txt$)|(rda$)|(pdf$)|(png$)")
    file.remove(to_remove)
    expect_true(length(to_remove) > 0)
    expect_true(all(!file.exists(to_remove)))
})
