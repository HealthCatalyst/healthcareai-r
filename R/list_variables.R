#' Takes a vector of variables and returns the unique variable in a string
#'
#' @param my_variables a vector of categorical variables
#' @return a string that lists all the unique elements of the vector in sentence form
#' 
list_variables <- function(my_variables) {
  if (!is.vector(my_variables)) {
    stop("not_vector")
  }
  my_unique_variables <- unique(my_variables)
  if (length(my_unique_variables) == 0) {
    stop("vector_length_0")
  } else if (length(my_unique_variables) == 1) {
    return(my_unique_variables)
  } else if (length(my_unique_variables) == 2) {
    return(paste(my_unique_variables, collapse = " and "))
  } else {
    lastIndex <- length(my_unique_variables)
    tmp <- paste(my_unique_variables[1:lastIndex - 1], collapse = ", ")
    return(paste(c(tmp, my_unique_variables[lastIndex]), collapse = ", and "))
  }
}
