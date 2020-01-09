#' Takes a vector or list of variables and returns the unique variables in a string
#'
#' @param my_variables a vector or list of variables
#' @return a string that lists all the unique elements of the vector or list in sentence form
#' @noRd
#'
list_variables <- function(my_variables) {
  if (!is.atomic(my_variables)) {
    stop("not_atomic")
  }
  my_unique_variables <- unique(my_variables)
  if (length(my_unique_variables) == 0) {
    stop("vector_length_0")
  } else if (length(my_unique_variables) == 1) {
    return(my_unique_variables)
  } else if (length(my_unique_variables) == 2) {
    return(paste(my_unique_variables, collapse = " and "))
  } else {
    last_index <- length(my_unique_variables)
    tmp <- paste(my_unique_variables[1:last_index - 1], collapse = ", ")
    return(paste(tmp, my_unique_variables[last_index], sep = ", and "))
  }
}
