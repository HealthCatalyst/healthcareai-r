# The tests in this file test the functions in 
# common-process-variable-recommendations.R

context("Checking modifiable process variable recommendation functions")

test_that("drop_repeated drops the correct rows", {
  # Build a list of dataframes with repeated values in "id" column
  d1 <- data.frame(id = c("A", "A", "B", "B", "C", "C"), x = 1:6)
  d2 <- data.frame(id = c("A", "B", "C", "A", "B", "C"), x = 1:6)
  d3 <- data.frame(id = c("A", "B", "C", "D", "E", "F"), x = 1:6)
  d4 <- data.frame(id = c("A", "A", "B", "B", "A", "B"), x = 1:6)
  df_list <- list(d1 = d1, d2 = d2, d3 = d3, d4 = d4)
  
  # Drop the repeated values in "id" column
  small_df_list <- drop_repeated(df_list = df_list, column_name = "id")
  
  # Check that output list has the same length as the original
  expect_length(small_df_list, length(df_list))
  # Check that the correct half of the rows were dropped for d1 and d2
  expect_equal(small_df_list$d1$x, c(1,3,5))
  expect_equal(small_df_list$d2$x, c(1,2,3))
  # Check that no rows were dropped for d3
  expect_equal(small_df_list$d3$x, 1:6)
  # Check that the correct 2/3 of rows were dropped for d4
  expect_equal(small_df_list$d4$x, c(1,3))
})

test_that("build_one_level_df works", {
  levels = c("A", "B", "C", "D")
  column_x <- factor(c("A", "C", "D", "A", "D"), levels = levels)
  level_C <- column_x[2]
  
  d <- data.frame(id = 1:5, 
                  x = column_x, 
                  y = c("red", "blue", "green", "white", "blue"))
  
  one_level_df <- build_one_level_df(dataframe = d,
                                     modifiable_variable = "x",
                                     level = level_C)
  
  # Check that column x is still a factor and has the correct levels
  expect_true(is.factor(one_level_df$x))
  expect_equal(levels(one_level_df$x), levels)
  # Check that id and y columns are unchanged
  expect_equal(one_level_df$id, d$id)
  expect_equal(one_level_df$y, d$y)
  # Check that the current_value and alt_value columns are correct
  expect_equal(one_level_df$current_value, as.character(d$x))
  expect_equal(one_level_df$alt_value, rep("C", times = nrow(d)))
})

test_that("permute_process_variables gives the right shape and column types", {
  # Build a toy dataframe
  d <- data.frame(id = 1:4, 
                  age = c(23, 46, 72, 18), 
                  gender = c("M", "F", "M", "F"),
                  flag = c("Y", "Y", "Y", "N"),
                  count = c(3, 5, 8, 13),
                  color = c("blue", "red", "green", "red"))
  # Get the factor levels for the variables which we choose to be modifiable
  # Note that flag is a factor, but not included in levels
  levels <- list(gender = levels(d$gender), color = levels(d$color))
  
  permuted_df <- permute_process_variables(dataframe = d,
                                           modifiable_variable_levels = levels)
  # Get expected dimensions
  number_of_levels <- length(unlist(levels))
  expected_rows <- number_of_levels*nrow(d)
  expected_columns <- ncol(d) + 3
  
  # Check the number of dimensions is correct
  expect_equal(dim(permuted_df), c(expected_rows, expected_columns))
  # Check that gender and color are still factors
  expect_true(is.factor(permuted_df$gender))
  expect_true(is.factor(permuted_df$color))
  # Check that current_value, alt_value, and process_variable_name columns are
  # character columns
  expect_true(is.character(permuted_df$current_value))
  expect_true(is.character(permuted_df$alt_value))
  expect_true(is.character(permuted_df$process_variable_name))
})

test_that("permute_process_variables gives the right values", {
  # Build a toy dataframe
  d <- data.frame(id = 1:4, 
                  age = c(23, 46, 72, 18), 
                  gender = c("M", "F", "M", "F"),
                  flag = c("Y", "Y", "Y", "N"),
                  count = c(3, 5, 8, 13),
                  color = c("blue", "red", "green", "red"))
  # Get the factor levels for the variables which we choose to be modifiable
  # Note that flag is a factor, but not included in levels
  levels <- list(gender = levels(d$gender), color = levels(d$color))
  
  permuted_df <- permute_process_variables(dataframe = d,
                                           modifiable_variable_levels = levels)
  
  # Get the number of levels
  number_of_levels <- length(unlist(levels))
  
  # Check age and flag columns are repeated but unchanged
  expect_equal(permuted_df$age, rep(d$age, times = number_of_levels))
  expect_equal(permuted_df$flag, rep(d$flag, times = number_of_levels))
  
  # Check values in a certain row
  expect_equal(permuted_df$gender[3], factor("F", levels = levels$gender))
  expect_equal(permuted_df$color[3], factor("green", levels = levels$color))
  expect_equal(permuted_df$current_value[3], "M")
  expect_equal(permuted_df$alt_value[3], "F")
  expect_equal(permuted_df$process_variable_name[3], "gender")
  # Check values in a different row
  expect_equal(permuted_df$gender[17], factor("M", levels = levels$gender))
  expect_equal(permuted_df$color[17], factor("red", levels = levels$color))
  expect_equal(permuted_df$current_value[17], "blue")
  expect_equal(permuted_df$alt_value[17], "red")
  expect_equal(permuted_df$process_variable_name[17], "color")
})