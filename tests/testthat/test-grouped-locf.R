context("Checking that the last observation is carried forward")

test_that("Data frame outputted is same as data frame inputted when no NAs are present", {
  dt = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.5,.6,.2,.2,.3,.7,.6),
                  ht=c(1,1,3,4,4,6,6),
                  date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt,'PersonID'), dt)
})

test_that("Observation carried forward with single NA", {
  dt = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.5,NA,.2,.2,.3,.7,.6),
                  ht=c(1,1,3,NA,4,6,6),
                  date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  dtresult = data.table(PersonID=c(1,1,2,2,3,3,3),
                         wt=c(.5,.5,.2,.2,.3,.7,.6),
                         ht=c(1,1,3,3,4,6,6),
                         date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                                '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt,'PersonID'), dtresult)
})

test_that("Observation carried forward with multiple NAs", {
  dt = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.5,.6,.2,.2,.3,NA,NA),
                  ht=c(1,1,3,4,4,NA,NA),
                  date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  dtresult = data.table(PersonID=c(1,1,2,2,3,3,3),
                        wt=c(.5,.6,.2,.2,.3,.3,.3),
                        ht=c(1,1,3,4,4,4,4),
                        date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                               '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt,'PersonID'), dtresult)
})

test_that("NA between two observations", {
  dt = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.5,.6,.2,.2,.3,NA,.4),
                  ht=c(1,1,3,4,4,NA,6),
                  date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  dtresult = data.table(PersonID=c(1,1,2,2,3,3,3),
                        wt=c(.5,.6,.2,.2,.3,.3,.4),
                        ht=c(1,1,3,4,4,4,6),
                        date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                               '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt,'PersonID'), dtresult)
})

test_that("Two observations with same time or sequence column value carried forward", {
  dt = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.5,.6,.2,.2,.3,.3,NA),
                  ht=c(1,1,3,4,4,4,NA),
                  date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/15/2015'))
  dtresult = data.table(PersonID=c(1,1,2,2,3,3,3),
                        wt=c(.5,.6,.2,.2,.3,.3,.3),
                        ht=c(1,1,3,4,4,4,4),
                        date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                               '01/01/2015','01/15/2015','01/15/2015'))
  expect_identical(groupedLOCF(dt,'PersonID'), dtresult)
})

test_that("Observation is still carried forward when time or sequence column goes back in value for some person", {
  dt1 = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.6,NA,.2,.2,.3,.3,.5),
                  ht=c(1,NA,3,4,4,4,5),
                  date=c('01/15/2015','01/01/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  dt1result = data.table(PersonID=c(1,1,2,2,3,3,3),
                        wt=c(.6,.6,.2,.2,.3,.3,.5),
                        ht=c(1,1,3,4,4,4,5),
                        date=c('01/15/2015','01/01/2015','01/01/2015','01/15/2015',
                               '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt1,'PersonID'), dt1result)

  dt2 = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(NA,.6,.2,.2,.3,.3,.5),
                  ht=c(NA,1,3,4,4,4,5),
                  date=c('01/15/2015','01/01/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  dt2result = data.table(PersonID=c(1,1,2,2,3,3,3),
                        wt=c(NA,.6,.2,.2,.3,.3,.5),
                        ht=c(NA,1,3,4,4,4,5),
                        date=c('01/15/2015','01/01/2015','01/01/2015','01/15/2015',
                               '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt2,'PersonID'), dt2result)
})

test_that("No observation carried forward when all values are NA", {
  dt = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.5,.6,NA,NA,.3,.3,.3),
                  ht=c(1,1,NA,NA,4,4,4),
                  date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  dtresult = data.table(PersonID=c(1,1,2,2,3,3,3),
                        wt=c(.5,.6,NA,NA,.3,.3,.3),
                        ht=c(1,1,NA,NA,4,4,4),
                        date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                               '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt,'PersonID'), dtresult)
})

test_that("Observation carried forward when observation type is string", {
  dt = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.5,.6,.4,.7,.3,.3,.3),
                  ht=c(1,1,5,9,4,4,4),
                  letter=c('a','b','d',NA,'e',NA,NA),
                  date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  dtresult = data.table(PersonID=c(1,1,2,2,3,3,3),
                        wt=c(.5,.6,.4,.7,.3,.3,.3),
                        ht=c(1,1,5,9,4,4,4),
                        letter=c('a','b','d','d','e','e','e'),
                        date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                               '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt,'PersonID'), dtresult)
})

test_that("Observation carried forward when observation type is boolean", {
  dt = data.table(PersonID=c(1,1,2,2,3,3,3),
                  wt=c(.5,.6,.4,.7,.3,.3,.3),
                  ht=c(1,1,5,9,4,4,4),
                  boolean=c(TRUE,FALSE,TRUE,NA,FALSE,NA,TRUE),
                  date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                         '01/01/2015','01/15/2015','01/30/2015'))
  dtresult = data.table(PersonID=c(1,1,2,2,3,3,3),
                        wt=c(.5,.6,.4,.7,.3,.3,.3),
                        ht=c(1,1,5,9,4,4,4),
                        boolean=c(TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE),
                        date=c('01/01/2015','01/15/2015','01/01/2015','01/15/2015',
                               '01/01/2015','01/15/2015','01/30/2015'))
  expect_identical(groupedLOCF(dt,'PersonID'), dtresult)
})
