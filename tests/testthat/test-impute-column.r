# context("Checking that columns are imputed correctly")

# test_that("Vectors aren't changed when no NAs are present", {
#   expect_identical(imputeColumn(c(1,2,3,4)), c(1,2,3,4))
#   expect_identical(imputeColumn(c(1L,2L,3L,4L)), c(1,2,3,4))
#   expect_identical(imputeColumn(c('a','b','c','a')), c('a','b','c','a'))
#   expect_identical(imputeColumn(c(TRUE,TRUE,FALSE,TRUE)), c("TRUE","TRUE","FALSE","TRUE"))
# })


# test_that("Handles vectors with only NAs", {
#   expect_error(imputeColumn(c(NA,NA,NA,NA)),'replacement has length zero')
#   expect_error(imputeColumn(c(NA)), 'replacement has length zero')
# })

# test_that("Numeric vectors replace single NA with average", {
#   expect_identical(imputeColumn(c(1,2,3,NA)), c(1,2,3,2))
#   expect_identical(imputeColumn(c(1,NA,3,2)), c(1,2,3,2))
#   expect_identical(imputeColumn(c(1,2,NA)), c(1,2,1.5))

#   expect_identical(imputeColumn(c(1L,2L,3L,NA)), c(1,2,3,2))
#   expect_identical(imputeColumn(c(1L,NA,3L,2L)), c(1,2,3,2))
#   expect_identical(imputeColumn(c(1L,2L,NA)), c(1,2,1.5))
# })

# test_that("Numeric vectors replace multiple NAs with average", {
#   expect_identical(imputeColumn(c(1,NA,3,NA)), c(1,2,3,2))
#   expect_identical(imputeColumn(c(NA,1,NA)), c(1,1,1))

#   expect_identical(imputeColumn(c(1L,NA,3L,NA)), c(1,2,3,2))
#   expect_identical(imputeColumn(c(NA,1L,NA)), c(1,1,1))
# })

# test_that("Factor vectors replace single NA with most frequent value when single most frequent value", {
#   expect_identical(imputeColumn(c('a','a','b',NA)), c('a','a','b','a'))
#   expect_identical(imputeColumn(c('b',NA,'c','c')), c('b','c','c','c'))
#   expect_identical(imputeColumn(c('ab','ab',NA)), c('ab','ab','ab'))

#   expect_identical(imputeColumn(as.factor(c('a','a','b',NA))), as.factor(c('a','a','b','a')))
#   expect_identical(imputeColumn(as.factor(c('b',NA,'c','c'))), as.factor(c('b','c','c','c')))
#   expect_identical(imputeColumn(as.factor(c('ab','ab',NA))), as.factor(c('ab','ab','ab')))
# })

# test_that("Factor vectors replace multiple NAs with most frequent value when single most frequent value", {
#   expect_identical(imputeColumn(c('a',NA,'a','b',NA)), c('a','a','a','b','a'))
#   expect_identical(imputeColumn(c(NA,'a',NA)), c('a','a','a'))

#   expect_identical(imputeColumn(as.factor(c('a',NA,'a','b',NA))), as.factor(c('a','a','a','b','a')))
#   expect_identical(imputeColumn(as.factor(c(NA,'a',NA))), as.factor(c('a','a','a')))
# })

# test_that("Factor vectors replace single NA with most frequent value when multiple most frequent values", {
#   expect_identical(imputeColumn(as.factor(c('a','a','b','b',NA))), as.factor(c('a','a','b','b','a')))
#   expect_identical(imputeColumn(as.factor(c(TRUE,NA,TRUE,FALSE,FALSE))), as.factor(c('TRUE','FALSE','TRUE','FALSE','FALSE')))
#   expect_identical(imputeColumn(as.factor(c('abc','bd','bd','abc','a',NA))), as.factor(c('abc','bd','bd','abc','a','abc')))
# })

# test_that("Factor vectors replace multiple NAs with most frequent value when multiple most frequent value", {
#   expect_identical(imputeColumn(as.factor(c('a',NA,'b',NA))), as.factor(c('a','a','b','a')))
#   expect_identical(imputeColumn(as.factor(c(NA,'f',NA,'d'))), as.factor(c('d','f','d','d')))
# })

# test_that("Data frames aren't changed when no NAs are present", {
#   dforiginal = data.frame(a=c(1,2,3,4),
#                           b=c('Y','N','Y','Y'),
#                           c=c(1,2,3,4))
#   df = data.frame(a=c(1,2,3,4),
#                   b=c('Y','N','Y','Y'),
#                   c=c(1L,2L,3L,4L))
#   df[] <- lapply(df, imputeColumn)
#   expect_identical(df, dforiginal)
# })

# test_that("Handles data frames with only NAs", {
#   df = data.frame(a=c(NA,NA,NA,NA),
#                   b=c(NA,NA,NA,NA))
#   expect_error(df[] <- lapply(df, imputeColumn),'replacement has length zero')
# })

# test_that("Data frames are correctly changed when NAs are present", {
#   dfimputedcorrectly1 = data.frame(a=c(1,2,3,2),
#                                    b=c('Y','N','Y','Y'),
#                                    c=c(1,2,3,2))
#   df1 = data.frame(a=c(1,2,3,NA),
#                    b=c('Y','N',NA,'Y'),
#                    c=c(1L,2L,3L,NA))
#   df1[] <- lapply(df1, imputeColumn)
#   expect_identical(df1, dfimputedcorrectly1)

#   dfimputedcorrectly2 = data.frame(a=c(1,2,3,2),
#                                    b=c('Y','N','N','N'),
#                                    c=c(1,2,3,2))
#   df2 = data.frame(a=c(1,NA,3,NA),
#                    b=c('Y','N',NA,NA),
#                    c=c(1L,NA,3L,NA))
#   df2[] <- lapply(df2, imputeColumn)
#   expect_identical(df2, dfimputedcorrectly2)
# })
