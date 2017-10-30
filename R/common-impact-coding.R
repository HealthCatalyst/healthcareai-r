#' @title 
#' Pre-Process High-Cardinality Variables
#' @description pphcv stands for pre-processing of high-cardinality variables. 
#' It is a method to reduce high-cardinality (many categories) variables into 
#' probabilities of the target class or expectations of the target. This allows 
#' the user to utilize variables with large numbers of categories and thus 
#' retain much of the explanatory power of those variables. 
#' 
#' For binary targets, the high-cardinality variables will be reduced to 
#' probabilities of the target positive class given each high-cardinality level.
#' For continuous targets, the high-cardinality variables will be reduced to 
#' expectations of the target variable given each high-cardinality level. For 
#' multiclass targets with n classes, each high-cardinality variable will be 
#' transformed into n - 1 attributes representing the probability of each target
#' class given each level of the high-cardinality variable. 
#' 
#' This technique was taken from a paper titled,"A preprocessing Scheme for High
#' Cardinality Categorical Attributes in Classification and Prediction 
#' Problems," written by Daniele Micci-Barreca, published in ACM SIGKDD 
#' Explorations Newsletter, Volume 3 Issue 1, July 2001, pages 27-32. See
#' references.
#' @param df A data frame that contains the high-cardinality variable/s and an 
#' outcome variable of interest.
#' @param target The variable/outcome of interest. Enter as a string.
#' @param high_card The high-cardinality variable/s. Enter as a string or vector
#' of strings, i.e. c("variable1", "variable2", "variable3")
#' @param type What type of variable is the target? Options are "binary",  
#' "continuous", and "multiclass".
#' @param pos_class If target is binary, what is the positive class? Enter as a 
#' string for a factor or character target and a number for a numeric target.
#' @param m A tuneable parameter that depends on the data domain and expected 
#' noise. If the expected noise is small, m should be small and vice versa. 
#' Defaults to 1.
#' @param multi_base If target is multiclass, multi_base gives the option to 
#' specify which class to use as the base category. If not specified the first
#' class will be used as the base level.
#' @param remove_high_card If TRUE, removes the original high-cardinality 
#' variable from the final output dataframe. Defaults to FALSE
#' @return A data frame that contains the new variables consisting of the
#' probabilities/expectations of the high-cardinality variables. Note: the
#' original high-cardinality variable/s will be removed from the final output if
#' specified using remove_high_card.
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @references \url{https://dl.acm.org/citation.cfm?id=507538}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' set.seed(10)
#' dat <- data.frame(Ycontinuous = sample(5:42, size = 14, replace = TRUE),
#'                   Ycategorical = sample(c("Y", "N"), size = 14, 
#'                                        replace = TRUE, prob = c(0.45, 0.55)),
#'                   YCat01 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0),
#'                   hcX = sample(letters[1:5], size = 14, replace = TRUE),
#'                   hcX2 = sample(letters[6:10], size = 14, replace = TRUE),
#'                   hcX3 = sample(letters[11:15], size = 14, replace = TRUE),
#'                   Ymulticlass = sample(c("blue", "green", "red"), size = 14, 
#'                                        replace = TRUE))
#'                
#' # Continuous target and one high_cardinality variable
#' pphcv(df = dat, target = "Ycontinuous", high_card = "hcX", 
#'       type = "continuous")
#'           
#' # Binary target and one high_cardinality variable
#' pphcv(df = dat, target = "Ycategorical", high_card = "hcX", pos_class = "Y", 
#'       type = "binary")
#'           
#' # Binary target coded as numeric(0, 1) and one high-cardinality variable 
#' # and specifying the pos_class.
#' pphcv(df = dat, target = "YCat01", high_card = "hcX", pos_class = 1, 
#'       type = "binary")
#'           
#' # Binary target coded as numeric(0, 1) and one high-cardinality variable and 
#' # not specifying the pos_class. Use type = "continuous". Should give same
#' # results as if type is binary and positive class is specified.
#' pphcv(df = dat, target = "YCat01",  high_card = "hcX", type = "continuous", 
#'       m = 1)
#' 
#' # Multiclass target and one high-cardinality variable
#' pphcv(df = dat, target = "Ymulticlass", high_card = "hcX", 
#'       type = "multiclass")
#'           
#' # Binary target and a vector of high-cardinality variables.
#' vec <- c("hcX", "hcX2", "hcX3")
#' pphcv(df = dat, target = "Ycategorical", high_card = vec, 
#'           type = "binary", pos_class = "Y")
#' 
#' # Multiclass target and a vector of high-cardinality variables and specify
#' # remove_high_card = TRUE
#' vec <- c("hcX", "hcX2", "hcX3")
#' pphcv(df = dat, target = "Ymulticlass", high_card = vec, type = "multiclass", 
#'       remove_high_card = TRUE)
pphcv <- function(df, target, high_card, type, pos_class = NULL, m = NULL,
                  multi_base = NULL, remove_high_card = FALSE) {
  
  for (i in high_card) {
    # For binary, if target is numeric, pos_class must also be numeric.
    if (type == "binary" && is.numeric(df[[target]]) && 
        !is.numeric(pos_class)) {
      stop("If specified binary target is numeric, pos_Class must also be 
           numeric. You may also use type = 'continuous' with no specified 
           pos_class")
    }
    # For binary, must have a pos_class specified
    if (type == "binary" && is.null(pos_class)) {
      stop("You must specify the positive class of your binary target")
    }
    # If any NAs are present, throw error that user must deal with them 
    # beforehand.
    if (anyNA(df[[target]]) || anyNA(df[[i]])) {
      stop("You must deal with NAs before reducing cardinality. Consider 
           re-coding all NAs in high_card into a new category 'missing'. 
           Also consider removingrows where target is missing.")
    }
    # If type is multiclass and the target is numeric, give a warning about
    # coercion to factor.
    if (type == "multiclass" && is.numeric(df[[target]])) {
      warning("Your specified target is multiclass and numeric. It was coerced
              into a factor for use.")
    }
    # If type is binary calculate the prior and the posterior probabilities
    if (type == "binary") {
      prior <- sum(df[[target]] == pos_class) / nrow(df)
      post <- table(df[[i]][df[[target]] == pos_class]) / table(df[[i]])
    } 
    # If type is continuous, calculate the prior and posterior expectations
    if (type == "continuous") {
      prior <- mean(df[[target]])
      post <- sapply(split(df[[target]], df[[i]]), mean)
    } 
    # If the tunable parameter m is null, set it to 0.5 as default.
    if (is.null(m)) {
      m = 1
    }
    # If type is binary or continuous...
    if (type == "binary" || type == "continuous") {
      # Calculate the shrinkage factor for each high-cardinality level
      shrinkage_factor <- table(df[[i]]) / (m + table(df[[i]]))
      # Calculate the probabilities/expectations for each high-cardinaltiy level
      low_card <- (shrinkage_factor*post) + ((1 - shrinkage_factor) * prior)
      # Create new variables in the dataframe that are the probabilities or
      # expectations of the high-cardinality levels.
      new <- paste(colnames(df[i]), "_new", sep = "")
      # Add the new variables to the dataframe for later output.
      df[[new]] <- low_card[df[[i]]]
      df[[new]] <- as.numeric(df[[new]])
    }
    # If type is multiclass...
    if (type == "multiclass") {
      # If multiclass target is character or numeric, convert to a factor.
      if (is.character(df[[target]]) || is.numeric(df[[target]])) {
        df[[target]] <- as.factor(df[[target]])
      }
      # Create subset of target levels leaving out multi_base as specified
      if (!is.null(multi_base)) {
        multi_levels = levels(df[[target]])[levels(df[[target]]) != multi_base]
        # Or if multi_base is null, use the first target level as the base
      } else {
        multi_levels = levels(df[[target]])[-1]
      }
      # Follow the same process for each non-base target level and each level of
      # the high-cardinality variable just as we do if the type is binary.
      for (k in multi_levels) {
        prior <- sum(df[[target]] == k) / nrow(df)
        post <- (table(df[[i]][df[[target]] == k])) / table(df[[i]])
        shrinkage_factor <- table(df[[i]]) / (m + table(df[[i]]))
        low_card <- (shrinkage_factor * post) + ((1 - shrinkage_factor) * prior)
        new <- paste(colnames(df[i]), "_", k, "_new", sep = "")
        df[[new]] <- low_card[df[[i]]]
        df[[new]] <- as.numeric(df[[new]])
      }
    }
    # If remove_high_card is specified, delete the high-cardinality variable/s.
    if (remove_high_card) {
      df[[i]] <- NULL
    }
  }
  # Return a new dataframe
  return(list(newdf = df))
}