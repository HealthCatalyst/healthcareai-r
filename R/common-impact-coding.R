#' @title 
#' Pre-Process High-Cardinality Variables
#' @description pphcv stands for pre-processing of high-cardinality variables. 
#' It is a method to reduce high-cardinality (many categories) variables into 
#' probabilities of the target class or expectations of the target. This allows 
#' the user to utilize variables with large numbers of categories and thus 
#' retain much of the explanatory power of those variables. This 
#' framework can be used in training and testing. If used in training it will 
#' also need to be applied to testing data. Must be used in training in order
#' to be used in testing.
#' 
#' For binary targets, the high-cardinality variable/s will be reduced to 
#' probabilities of the target positive class given each high-cardinality level 
#' otherwise known as the conditional probability, P(Y|X=x).
#' 
#' For continuous targets, the high-cardinality variable/s will be reduced to 
#' expectations of the target variable given each high-cardinality level. Also
#' known as the conditional expectation, E[Y|X=x] 
#' 
#' For multiclass targets with n classes, each high-cardinality variable will be 
#' transformed into n - 1 attributes representing the probability of each target
#' class given each level of the high-cardinality variable. The user may specify
#' the base class.
#' 
#' This technique was taken from a paper titled,"A preprocessing Scheme for High
#' Cardinality Categorical Attributes in Classification and Prediction 
#' Problems," written by Daniele Micci-Barreca, published in ACM SIGKDD 
#' Explorations Newsletter, Volume 3 Issue 1, July 2001, pages 27-32. See
#' references.
#' @param df A data frame that contains the high-cardinality variable/s and an 
#' outcome/target variable of interest.
#' @param high_card The high-cardinality variable/s. Enter as a string or vector
#' of strings, i.e. c("variable1", "variable2", "variable3")
#' @param type What type of variable is the target? Options are "binary",  
#' "continuous", and "multiclass". Enter as a string.
#' @param target The variable target/outcome of interest. Enter as a string. 
#' Only used for training data.
#' @param pos_class If target is binary, what is the positive class? Enter as a 
#' string for a factor or character target and a number for a numeric target.
#' @param m_tuning_param A tuneable parameter that depends on the data domain 
#' and expected noise. If the expected noise is small, m should be small and 
#' vice versa. Defaults to 1. Not used when applying mappings to testing data.
#' @param multi_base If target is multiclass, multi_base gives the option to 
#' specify which class to use as the base category. If not specified the first
#' class will be used as the base level.
#' @param remove_high_card If TRUE, removes the high-cardinality variable from 
#' the final dataframe output. Defaults to FALSE.
#' @param mapping If user wants to do impact coding on testing/deployment data,
#' mapping will use the computed probabilities or expectations from the
#' development step. Only used during testing phase.
#' @return A list that contains the new data frame with the probabilities or 
#' expectations of the high-cardinality variable/s and that also contains the 
#' mappings of each category level.  Mappings are used in deployment/testing.
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @references \url{https://dl.acm.org/citation.cfm?id=507538}
#' @seealso \code{\link{healthcareai}}
#' @examples
#' # These examples show how to pre-process training data and then apply the 
#' # mappings to testing data for several different parameter/ data types
#' # combinations
#' 
#' set.seed(10)
#' train <- data.frame(Ycontinuous = sample(5:42, size = 14, replace = TRUE),
#'                     Ycategorical = sample(c("Y", "N"), size = 14, 
#'                                        replace = TRUE, prob = c(0.45, 0.55)),
#'                     YCat01 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0),
#'                     hcX = sample(letters[1:5], size = 14, replace = TRUE),
#'                     hcX2 = sample(letters[6:10], size = 14, replace = TRUE),
#'                     hcX3 = sample(letters[11:15], size = 14, replace = TRUE),
#'                     Ymulticlass = sample(c("blue", "green", "red"), 
#'                                            size = 14, replace = TRUE))
#' 
#' set.seed(12)
#' test <- data.frame(hcX = sample(letters[1:5], size = 14, replace = TRUE),
#'                    hcX2 = sample(letters[6:10], size = 14, replace = TRUE),
#'                    hcX3 = sample(letters[11:15], size = 14, replace = TRUE))
#' 
#' # Continuous target and one high_cardinality variable
#' f <- pphcv(df = train, target = "Ycontinuous", high_card = "hcX", 
#'            type = "continuous", m_tuning_param = 0.5)
#'       
#' f1 <- pphcv(df = test, high_card = "hcX", mapping = f$mappings, 
#'             type = "continuous", remove_high_card = TRUE)
#'           
#' # Binary target and one high_cardinality variable
#' g <- pphcv(df = train, target = "Ycategorical", high_card = "hcX", 
#'            pos_class = "Y", type = "binary")
#'            
#' g1 <- pphcv(df = test, high_card = "hcX", mapping = g$mappings, 
#'             type = "binary")
#'           
#' # Binary target coded as numeric(0, 1) and one high-cardinality variable 
#' # and specifying the pos_class.
#' h <- pphcv(df = train, target = "YCat01", high_card = "hcX", pos_class = 1, 
#'            type = "binary", remove_high_card = FALSE)
#' h1 <- pphcv(df = test, high_card = "hcX", mapping = h$mappings, 
#'             type = "binary")
#'             
#' #Binary target coded as numeric(0, 1) and one high-cardinality variable and 
#' #not specifying the pos_class. Use type = "continuous". Should give same
#' #results as if type is binary and positive class is specified.
#' I <- pphcv(df = train, target = "YCat01",  high_card = "hcX", 
#'            type = "continuous")
#' I1 <- pphcv(df = test, high_card = "hcX", mapping = I$mappings,  
#'             type = "continuous")
#' 
#' # Multiclass target and one high-cardinality variable
#' j <- pphcv(df = train, target = "Ymulticlass", high_card = "hcX", 
#'            type = "multiclass")
#' j1 <- pphcv(df = test, high_card = "hcX", mapping = j$mappings, 
#'            type = "multiclass")
#'       
#'           
#' # Continuous target and several high-cardinality variables
#' vec <- c("hcX", "hcX2", "hcX3")
#' k <- pphcv(df = train, target = "Ycontinuous", high_card = vec, 
#'            type = "continuous", remove_high_card = FALSE)
#' k1 <- pphcv(df = test, high_card = vec, mapping = k$mappings, 
#'             type = "continuous")
#'
#' # Binary target and several high_cardinality variables
#' vec <- c("hcX", "hcX2", "hcX3")
#' L <- pphcv(df = train, target = "Ycategorical", high_card = vec, 
#'            pos_class = "Y", type = "binary", remove_high_card = FALSE)
#' L1 <- pphcv(df = test, high_card = vec, mapping = L$mappings, 
#'             type = "binary")
#'             
#' # Multiclass target and several high-cardinality variables
#' vec <- c("hcX", "hcX2", "hcX3")
#' O <- pphcv(df = train, target = "Ymulticlass", high_card = vec, 
#'            type = "multiclass")
#' O1 <- pphcv(df = test, high_card = vec, mapping = O$mappings, 
#'             type = "multiclass")
pphcv <- function(df, target = NULL, high_card, type, pos_class = NULL, 
                  m_tuning_param = NULL, multi_base = NULL, 
                  remove_high_card = FALSE, mapping = NULL) {
  
  map_list <- list()
  
  # If target is present, print messages and throw errors if necessary
  if (!is.null(target)) {
    message("Pre-processing training data")
    
    # Check for target in the dataframe
    if (!target %in% colnames(df)) {
      stop("Your specified target/outcome is not in your given dataframe")
    }
    
    # If target is present and type is binary
    if (type == "binary") {
      
      # If pos_class is present and pos_class is not in the specified target
      if (!is.null(pos_class) && !pos_class %in% df[[target]]) {
        stop("Your specified pos_class is not in your given target")
      }
      
      # If target has more than two classes, throw an error
      if (length(table(df[[target]])) > 2) {
        stop("Your specified target is not binary. Make sure you have only ",
             "two classes and that they are all numeric or all strings.")
      }
    }
    
    # If type is multiclass and the target is numeric, give a warning about
    # coercion to factor.
    if (type == "multiclass" && is.numeric(df[[target]])) {
      warning("Your specified target is multiclass and numeric. It was ",
              "coerced into a factor for use.")
    }
    
    # Advise to drop the rows where there are NAs
    if (base::anyNA(df[[target]])) {
      stop("You must deal with NAs before reducing cardinality. Consider ", 
           "removing rows where target is missing.")
    }
  }
  
  # If mapping is present, print a nice message to tell the user
  if (!is.null(mapping)) {
    message("Applying mappings to testing data")
    
    # Raise an error if user gives m_tuning_param durting testing phase
    if (!is.null(m_tuning_param)) {
      stop("When applying mappings in testing phase, do not specify an ",
           "m_tuning_param.")
    }
    
    # Raise an error if user tries to give pos_class in testing phase
    if (!is.null(pos_class)) {
      stop("When applying mappings in testing phase, do not specify a ", 
           "pos_class.")
    }
    
    # Raise an error if user tries to give multi_base in testing phase
    if (!is.null(multi_base)) {
      stop("When using mappings in testing phase, do not specify a multi_base")
    }
    
    # Raise an error if user gives different type in training than testing.
    if (mapping[["Type"]] != type) {
      stop("Your specified type does not match the type with which your ",
           "mappings were created.  Make the type in training and testing the ",
           "same.")
    }
  }
  
  # If Mapping is not present and there is no target, throw an error.
  if (is.null(mapping) && is.null(target)) {
    stop("If mapping is not specified, you must specify a target/outcome.")
  }
  
  # If mapping and target are included, throw an error to inform user.
  if (!is.null(mapping) && !is.null(target)) {
    stop("Mapping and target cannot be included in the same step for training ",
         "or testing. Which step are you on?")
  }
  
  # For binary, must have a pos_class specified
  if (type == "binary" && is.null(pos_class) && is.null(mapping)) {
    stop("You must specify the positive class of your binary target.")
  }
  
  for (i in high_card) {
    
    # If specified high_card variable/s not in data, throw an error
    if (!i %in% colnames(df)) {
      stop(paste(high_card, "is not in your data", sep = " "))
    }
    
    # If any NAs are present in the high-cardinality variable, give error 
    # message and advise to recode NAs to missing.
    if (base::anyNA(df[[i]])) {
      stop("you must deal with NAs before reducing cardinality. Consider ", 
           "re-coding all NAs in high_card into a new category 'missing'.")
    }
    
    # If mapping is not present, calculate prior, posterior, and shrinkage fact.
    if (is.null(mapping)) {
      
      # If type is binary calculate the prior and the posterior probabilities
      if (type == "binary") {
        prior <- base::sum(df[[target]] == pos_class) / base::nrow(df)
        post <- base::table(df[[i]][df[[target]] == pos_class]) / 
          base::table(df[[i]])
      }
      
      # If type is continuous, calculate the prior and posterior probabilities
      if (type == "continuous") {
        prior <- base::mean(df[[target]])
        post <- base::sapply(split(df[[target]], df[[i]]), base::mean)
      }
    }
    
    # If the tunable parameter m is null, set to 1 as default.
    if (is.null(mapping) && is.null(m_tuning_param)) {
      m_tuning_param = 1
    }
    
    # Calculate the shrinkage factor for each high-cardinality level
    if (is.null(mapping)) {
      shrinkage_factor <- base::table(df[[i]]) / 
        (m_tuning_param + base::table(df[[i]])) 
    }
    
    # If binary or continuous, do the impact coding
    if (type == "binary" || type == "continuous") {
      
      # Calculate the probabilities/expectations for each high-cardinaltiy level
      if (is.null(mapping)) {
        sub_post <- post[base::names(post) %in% base::names(shrinkage_factor)]
        low_card <- 
          (shrinkage_factor*sub_post) + ((1 - shrinkage_factor) * prior)
        map_list[[base::paste(base::colnames(df[i]), "_low_card", sep = "")]] <- 
          low_card
      } else if (!is.null(mapping)) {
        low_card <- mapping[[base::paste(base::colnames(df[i]), "_low_card", 
                                         sep = "")]]
      }
      
      map_list[[base::paste(base::colnames(df[i]), "_low_card", sep = "")]] <- 
        low_card
      
      # Create new variables in the dataframe that are the probabilities or
      # expectations of the high-cardinality levels.
      new <- base::paste(base::colnames(df[i]), "_new", sep = "")
      
      # Add the new variables to the dataframe for later output.
      df[[new]] <- low_card[as.character(df[[i]])]
      df[[new]] <- as.numeric(df[[new]])
    }
    
    # If type is multiclass...
    if (type == "multiclass") {
      if (is.null(mapping)) {
        
        # If multiclass target is character or numeric, convert to a factor.
        if (is.character(df[[target]]) || is.numeric(df[[target]])) {
          df[[target]] <- as.factor(df[[target]])
        }
        
        # Create subset of target levels leaving out multi_base as specified
        if (!is.null(multi_base)) {
          multi_levels <-
            base::levels(df[[target]])[base::levels(df[[target]]) != multi_base]
          
          # Or if multi_base is null, use the first target level as the base
        } else {
          multi_levels = base::levels(df[[target]])[-1]
        }
      } else {
        multi_levels = mapping[["multi_levels"]]
      }
      
      # Follow the same process for each non-base target level and each level of
      # the high-cardinality variable just as we do if the type is binary.
      for (k in multi_levels) {
        if (is.null(mapping)) {
          prior <- base::sum(df[[target]] == k) / base::nrow(df)
          post <- (base::table(df[[i]][df[[target]] == k])) / 
            base::table(df[[i]])
          low_card <- (shrinkage_factor * post) + 
            ((1 - shrinkage_factor) * prior)
        } else if (!is.null(mapping)) {
          low_card <- 
            mapping[[base::paste(base::colnames(df[i]), "_", k, "_low_card", 
                                 sep = "")]]
        }
        map_list[[base::paste(base::colnames(df[i]), "_", k, "_low_card", 
                              sep = "")]] <- low_card
        new <- base::paste(base::colnames(df[i]), "_", k, "_new", sep = "")
        df[[new]] <- low_card[as.character(df[[i]])]
        df[[new]] <- as.numeric(df[[new]])
      }
      map_list[["multi_levels"]] <- multi_levels
    }
    
    # If remove_high_card is specified, delete the high-cardinality variable/s.
    if (remove_high_card) {
      df[[i]] <- NULL
    }
  }
  
  # Put the type into the mappings list
  map_list[["Type"]] <- type
  
  # Return a new dataframe
  return(list(newdf = df, mappings = map_list))
  
}
