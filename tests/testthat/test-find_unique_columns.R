context("Testing find_unique_columns")


# Setup ------------------------------------------------------------------------
dirty_data_frame <- function() {
  return(
    data.frame(
      id_field = c("A", "B", "C", "D"),
      test1_field = c(10, 20, 30, 40),
      test2_field = c(100, 200, 300, 300),
      test3_field = c("A1", "B1", "B1", "D1"),
      test4_field = c("AA", "BB", "CC", "DD")
    ))
  }

clean_data_frame <- function() {
  return(data.frame(
    id_field = c("A", "A", "B", "B"),
    test1_field = c(1, 2, 1, 2)
  ))
  }

empty_data_frame <- function() {
  return(data.frame())
  }

character_factor_data_frame <- function() {
  return(data.frame(
    character_field = letters,
    factor_field = factor(letters),
    strings_as_factors = FALSE
  ))
}

# Tests ------------------------------------------------------------------------
test_that("Data frame that contains no columns or rows returns nothing", {
  expect_equal(character(), find_unique_columns(empty_data_frame()))
  })

test_that("Data frame with no columns containing entirely unique values
          should return nothing", {
            expect_equal(character(), find_unique_columns(clean_data_frame()))
            })

test_that(
  "Data frame with two non-numeric columns containing entirely unique values
  should return the column names of the columns with entirely unique values", {
    expect_equal(c("id_field", "test4_field"),
                 find_unique_columns(dirty_data_frame()))
  })

test_that(
  "Data frame with two non-numeric columns containing entirely unique values
  should return the column names of the columns with entirely unique values", {
    expect_equal(c("id_field", "test4_field"),
                 find_unique_columns(dirty_data_frame()))
  })

test_that(
  "Data frame with factors or characters should work the same with both", {
    expect_equal(c("character_field", "factor_field"),
                 find_unique_columns(character_factor_data_frame()))
  })

test_that("date columns aren't captured by find_unique_columns", {
  d <- tibble::tibble(adate = as.Date("2018-01-01"), chr = "A")
  expect_equal("chr", find_unique_columns(d))
})

test_that("find_columns_to_ignore works right", {
  expect_warning(t1 <- find_columns_to_ignore(dirty_data_frame()),
                 regexp = "will be ignored")
  expect_setequal(c("id_field", "test4_field"), t1)
  expect_warning(t2 <- find_columns_to_ignore(dirty_data_frame(), "id_field"),
                 regexp = "test4_field")
  expect_equal(t2, "test4_field")
  expect_warning(t3 <- find_columns_to_ignore(dirty_data_frame(),
                                              c("id_field", "test4_field")),
                 regexp = NA)
  expect_equal(t3, character())
  expect_warning(t4 <- find_columns_to_ignore(clean_data_frame()),
                 regexp = NA)
  expect_equal(t4, character())
})

test_that("DTS will not be included in unique columns", {
  d <-
    dirty_data_frame() %>%
    mutate(
      test5_DTS = c("Oct", "Jan", "Feb", "March")
    )

  actual_unique_columns <- find_unique_columns(d)
  expect_false("test5_DTS" %in% actual_unique_columns)
  expect_equal(actual_unique_columns, c("id_field", "test4_field"))
})
