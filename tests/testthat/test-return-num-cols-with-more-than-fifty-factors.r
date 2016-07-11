context("Checking number of columns with greater than fifty factors in a dataframe")

test_that("0 is returned when there are zero columns with greater than fifty factors", {
  df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q',
                      'r','s','t','u','v','w','x','y','z','aa','bb','cc','dd','ee','ff','gg',
                      'hh','ii','jj','kk','ll','mm','nn','oo','pp','qq','rr','ss','tt','uu',
                      'vv','ww','aa','aa'))
  numCols = ReturnNumColsWithMoreThanFiftyFactors(df)

  expect_equal(numCols, 0)
})

test_that("1 is returned when there is one column with greater than fifty factors", {
  df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  b=c('a','b',NA,'c','d','e','f','g','h','i','j','k','l','m','n','o','p','q',
                      'r','s','t','u','v','w','x','y','z','aa','bb','cc','dd','ee','ff','gg',
                      'hh','ii','jj','kk','ll','mm','nn','oo','pp','qq','rr','ss','tt','uu',
                      'vv','ww','xx','yy'))
  numCols = ReturnNumColsWithMoreThanFiftyFactors(df)

  expect_equal(numCols, 1)
})

test_that("Correct number of columns is returned when there are more than one columns with greater than fifty factors", {
  df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  b=c(NA,'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q',
                      'r','s','t','u','v','w','x','y','z','aa','bb','cc','dd','ee','ff','gg',
                      'hh','ii','jj','kk','ll','mm','nn','oo','pp','qq','rr','ss','tt','uu',
                      'vv','ww','xx','yy'),
                  b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q',
                      'r','s','t','u','v','w','x','y','z','aa','bb','cc','dd','ee','ff','gg',
                      'hh','ii','jj','kk','ll','mm','nn','oo','pp','qq','rr','ss','tt','uu',
                      'vv','ww','xx','yy',NA))
  numCols = ReturnNumColsWithMoreThanFiftyFactors(df)

  expect_equal(numCols, 2)
})
