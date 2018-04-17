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
    factor_field = factor(letters)
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
