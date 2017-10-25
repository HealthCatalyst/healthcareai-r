#' @title
#' Analyze points in time to determine whether or not Nelson Rule 1 was violated
#' @description
#' Search within a data frame to find violations of Nelson Rule 1.
#' Nelson Rule 1: One point is more than 3 standard deviations from the mean.
#' @param df A data frame
#' @param measure_col A string denoting the column that contains the numeric
#' value to be evaluated by the nelsonRule1 function
#' @param date_col A string denoting the date column used for evaluating
#' Nelson Rule 1 over time
#' @return A data frame containing the date, measure value, uppper control
#' limit, lower control limit, a flag indicating whether or not a rule violation
#' occured and a description of the violation.
#'
#' @importFrom stats aggregate formula
#' @importFrom utils tail
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' \url{https://en.wikipedia.org/wiki/Nelson_rules}
#' @seealso \code{\link{healthcareai}}
#' @examples
#'date <- seq.Date(from = as.Date('2016-01-03'), length.out = 52, by = 'week')
#'set.seed(34)
#'measureValue <- rnorm(length(date), mean = 100, sd = 15)
#'
#'# Alter some measureValues to be at least 3 standard deviations from the mean
#'# so they can be displayed as violation examples.
#'measureValue[9] <- 179
#'measureValue[19] <- 22
#'measureValue[47] <- 177
#'
#'d <- data.frame(date, measureValue)
#'
#'df <- nelsonRule1(df = d, measureCol = 'measureValue', dateCol = 'date')
#'
#'df
#'
#'ggplot(data = df
#'  ,aes(x = date
#'  ,y = measure)) +
#'  geom_line() +
#'  geom_hline(data = unique(df$ucl), yintercept = unique(df$ucl)) +
#'  geom_hline(data = unique(df$lcl), yintercept = unique(df$lcl)) +
#'  geom_hline(aes(yintercept = mean(df$measure)), linetype = 'dashed') +
#'  geom_point(data = df[df$violationFLG == TRUE,]
#'    ,aes(color = violationFLG))

nelsonRule1 <- function(df, measure_col, date_col, plot_flg = TRUE) {
  library(ggplot2)
  
  # Check to make sure that df is a dataframe
  if (!(is.data.frame(df))) {
    stop('df must be a dataframe.')
  }
  
  # Check to make sure that date_col is a date
  if (!(class(df[[date_col]]) == "Date")) {
    stop('date_col must be a date.')
  }
  
  # Check to make sure that date_col is a date
  if (!(is.numeric(df[[measure_col]]))) {
    stop('measure_col must be numeric.')
  }
  
  ucl <- mean(df[[measure_col]]) + 3 * sd(df[[measure_col]])
  lcl <- mean(df[[measure_col]]) - 3 * sd(df[[measure_col]])
  
  violationFLG <- as.numeric(df[[measure_col]] > ucl | df[[measure_col]] < lcl)
  
  violationDSC <- ifelse(df[[measure_col]] > ucl, 'more than 3 standard deviations above the mean'
    ,ifelse(df[[measure_col]] < lcl, 'more than 3 standard deviations below the mean',NA))
  
  out <- list()
  
  df <- data.frame(date = df[[date_col]]
    ,measure = df[[measure_col]]
    ,ucl = ucl
    ,lcl = lcl
    ,violationFLG = violationFLG
    ,violationDSC = violationDSC)
  
  colnames(df)[1:2] <- c(date_col,measure_col)
  
  ifelse(plot_flg == TRUE,
    out$p <- ggplot2::ggplot(data = df
      ,aes(x = date
        ,y = measureValue)) +
      geom_line() +
      geom_hline(data = unique(df$ucl), yintercept = unique(df$ucl)) +
      geom_hline(data = unique(df$lcl), yintercept = unique(df$lcl)) +
      geom_hline(aes(yintercept = mean(measureValue)), linetype = 'dashed') +
      geom_point(data = df[df$violationFLG == TRUE,]
        ,color = 'red'),
    out$p <- NA
  )
  
  #return(p)
  
  out$dfViolations <- df[df$violationFLG == 1,]
  
  return(out)
}

