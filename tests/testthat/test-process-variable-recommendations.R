# The tests in this file test the functions in 
# common-process-variable-recommendations.R and their integration into random 
# forest and lasso deployment.

# Tests of common-process-variable-recommendations.R functions -----------------

context("Checking modifiable process variable recommendations")

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

# Tests of integration with random forest and lasso deployment -----------------

# This test checks the behavior when the modifiable variable parameters are 
# misspecified
#   1. Check that an error is triggered if the modifiable variables are 
#      specified without also specifying if smaller predictions are desired
#   2. Check that a warning is triggered if the smaller predictions desired
#      parameter is set without also specifying the modifiable variables
#   3. Check that a warning is triggered if some of the modifiable variables 
#      are not present in the data
#   4. Check that modifiable variables not present in the data are removed
#   5. Check that a warning is triggered if none of the modifiable variables 
#      are present in the data
#   6. Check that the modifiable process variable parameter is reset to NULL if
#      none of the modifiable variables is present in the data 
#   7. Check that a warning is triggered if oen of the modifiable variables is
#      not categorical
#   8. Check that non-categorical modifiable variables are removed.
test_that("process variable parameter mismatch throws warnings and errors", {
  # Build a toy dataset
  n <- 250
  d <- data.frame(id = 1:n, 
                  x = rnorm(n),
                  y = sample(c("A", "B", "C"), replace = TRUE, size = n),
                  z = sample(c("pie", "cake"), replace = TRUE, size = n))
  # Assign a response variable
  temp <- d$x + 
    1.5 * (d$y == "A") - 1.5 * (d$y == "C") +
    (d$z == "pie") - (d$z == "cake")
  d$target <- ifelse(temp + rnorm(n) > 0, "Y", "N")
  
  # Develop a random forest model
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- d
  p$type <- "classification"
  p$predictedCol <- "target"
  p$grainCol <- "id"
  p$cores <- 1
  capture.output(rf <- RandomForestDevelopment$new(p))
  capture.output(rf$run())
  
  # Set up development parameters including modifiableProcessVariables, but 
  # not smallerPredictionsDesired
  p2 <- SupervisedModelDeploymentParams$new()
  p2$df <- d
  p2$type <- "classification"
  p2$predictedCol <- "target"
  p2$grainCol <- "id"
  p2$cores <- 1
  p2$modifiableProcessVariables <- c("y", "z")
  
  # 1. Check that error is thrown if the modifiableProcessVariables parameter is 
  # specified without aslso specifying the smallerPredictionsDesired parameter
  expect_error(capture.output(rfD <- RandomForestDeployment$new(p2)),
               "The modifiableProcessVariables parameter was specified ",
               "without also specifying the smallerPredictionsDesired ",
               "parameter.")
  
  # Add smallerPredictionsDesired and remove modifiableProcessVariables
  p2$modifiableProcessVariables <- NULL
  p2$smallerPredictionsDesired <- TRUE
  
  # 2. Check that a warning is thrown if the smallerPredictionsDesired parameter
  # is specified without aslso specifying the modifiableProcessVariables 
  # parameter
  expect_warning(capture.output(rfD <- RandomForestDeployment$new(p2)),
                 "The smallerPredictionsDesired parameter was specified ",
                 "without also specifying the modifiableProcessVariables ", 
                 "parameter.\nThe smallerPredictionsDesired parameter will be ",
                 "ignored.")
  
  # Add modifiable variables not present in the data
  p2$modifiableProcessVariables <- c("y", "z", "non-existant", "ghost")
  
  # 3. Check that a warning is triggered if new modifiable process variables 
  # are introduced
  expect_warning(capture.output(rfD <- RandomForestDeployment$new(p2)),
                 "Some of the modifiable process variables specified are not ",
                 "present in the data. Mystery variables:\n",
                 " - non-existant\n",
                 " - ghost\n",
                 "These modifiable variables will not be used.")
  
  # 4. Check that the extra modifiable process variables have been removed
  expect_equal(rfD$params$modifiableProcessVariables, c("y", "z"))
  
  p2$modifiableProcessVariables <- c("non-existant", "ghost")
  
  # 5. Check that a warning is triggered if none of the modifiable variables is
  # actually present in the data
  expect_warning(capture.output(rfD <- RandomForestDeployment$new(p2)),
                 "Some of the modifiable process variables specified are not ",
                 "present in the data. Mystery variables:\n",
                 " - non-existant\n",
                 " - ghost\n",
                 "These modifiable variables will not be used.")
  
  # 6. Check that the modifiable process variables is reset to null if none of 
  # the specified variables is actually present in the data.
  expect_null(rfD$params$modifiableProcessVariables)
  
  p2$modifiableProcessVariables <- c("x", "y", "z")
  
  # 7. Check that a warning is triggered if one of the modifiable variables is
  # not actually a categorical variable.
  expect_warning(capture.output(rfD <- RandomForestDeployment$new(p2)),
                 "Modifiable process variables must be categorical variables. ",
                 "The following vriables are not categorical and will not be ",
                 "used:\n",
                 " - x")
  
  # 8. Check that the extra non-categorical process variables have been removed
  expect_equal(rfD$params$modifiableProcessVariables, c("y", "z"))
})

# This test checks that modifiable variables interact correctly with variables
# which have a coefficient of 0 in the lasso:
#   1. Check that variables with nonzero coefficients are corretly recorded
# If one modifiable variable has zero coefficient
#   2. Check that a warning is triggered 
#   3. Check that the variable is dropped from the modifiable variable list
# If all modifiable variables have zero coefficient
#   4. Check that a warning is triggered
#   5. Check that the modifiable variable list is reset to NULL
test_that("modifiable variables behave well with lasso variable selection", {
  # Build a dataset with one useless categorical variable
  d <- data.frame(id = 1:12,
                  x = c(0, 2, 1, 2, 0, 3, 0, 3, 3, 4, 2, 4),
                  y = c("down", "down", "down", "down", "up", "down",
                        "up", "down", "up", "up", "up", "down"),
                  useless = rep(c("huh", "meh"), times = 6), # lasso will ignore
                  target = c(rep("N", times = 6), rep(c("Y"), times = 6)))
  # Make the dataset large enough to avoid triggering certain warnings
  d <- d[rep(1:12, times = 5), ]
  
  # Develop a lasso forest model
  p <- SupervisedModelDevelopmentParams$new()
  p$df <- d
  p$type <- "classification"
  p$predictedCol <- "target"
  p$grainCol <- "id"
  p$cores <- 1
  capture.output(lasso <- LassoDevelopment$new(p))
  # supress warning that is unrelated to the unit test
  ignoreSpecWarn(capture.output(lasso$run()), 
                 wRegexps = "glm.fit: fitted probabilities numerically 0 or 1")
  
  # 1. Check that the useless variable is dropped
  expect_equal(lasso$modelInfo$usedVariables, c("x", "y"))
    
  # Set up development parameters with one useless modifiable variable
  p2 <- SupervisedModelDeploymentParams$new()
  p2$df <- d
  p2$type <- "classification"
  p2$predictedCol <- "target"
  p2$grainCol <- "id"
  p2$cores <- 1
  p2$modifiableProcessVariables <- c("y", "useless")
  p2$smallerPredictionsDesired <- TRUE
  
  # 2. Check warning of useless variable being dropped is triggered
  expect_warning(capture.output(lassoD <- LassoDeployment$new(p2)), 
                 "The following variables have coefficients of 0 in the lasso ",
                 "model and will not be used as modifiable variables:\n",
                 " - useless")
  # 3. Check that useless variable is removed from list of modifiable variables
  expect_equal(lassoD$params$modifiableProcessVariables, c("y"))
  
  
  # Only use useless variable in modifiable variables
  p2$modifiableProcessVariables <- c("useless")
  
  # 4. Check warning of useless variable being dropped is triggered
  expect_warning(capture.output(lassoD <- LassoDeployment$new(p2)), 
                 "The following variables have coefficients of 0 in the lasso ",
                 "model and will not be used as modifiable variables:\n",
                 " - useless")
  # 5. Check that modifiableProcessVariables is reset to NULL
  expect_null(lassoD$params$modifiableProcessVariables)
})