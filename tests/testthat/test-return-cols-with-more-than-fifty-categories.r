context("Checking list of columns with greater than fifty categories in a dataframe")

test_that("Correct list is returned when there are zero columns with greater than fifty factors", {
  df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  b=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q',
                      'r','s','t','u','v','w','x','y','z','aa','bb','cc','dd','ee','ff','gg',
                      'hh','ii','jj','kk','ll','mm','nn','oo','pp','qq','rr','ss','tt','uu',
                      'vv','ww','aa','aa'))
  colList = returnColsWithMoreThanFiftyCategories(df)

  expect_equal(colList, vector('character'))
})

test_that("Correct list is returned when there is one column with greater than fifty factors", {
  df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  b=c('a','b',NA,'c','d','e','f','g','h','i','j','k','l','m','n','o','p','q',
                      'r','s','t','u','v','w','x','y','z','aa','bb','cc','dd','ee','ff','gg',
                      'hh','ii','jj','kk','ll','mm','nn','oo','pp','qq','rr','ss','tt','uu',
                      'vv','ww','xx','yy'))
  colList = returnColsWithMoreThanFiftyCategories(df)

  expect_equal(colList, c('b'))
})

test_that("Correct list is returned when there are more than one columns with greater than fifty factors", {
  df = data.frame(a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  b=c(NA,'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q',
                      'r','s','t','u','v','w','x','y','z','aa','bb','cc','dd','ee','ff','gg',
                      'hh','ii','jj','kk','ll','mm','nn','oo','pp','qq','rr','ss','tt','uu',
                      'vv','ww','xx','yy'),
                  c=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q',
                      'r','s','t','u','v','w','x','y','z','aa','bb','cc','dd','ee','ff','gg',
                      'hh','ii','jj','kk','ll','mm','nn','oo','pp','qq','rr','ss','tt','uu',
                      'vv','ww','xx','yy',NA))
  colList = returnColsWithMoreThanFiftyCategories(df)

  expect_equal(colList, c('b','c'))
})
