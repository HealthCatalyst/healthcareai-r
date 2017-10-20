#' @title
#' Analyze points in time to determine whether or not Nelson Rule 1 was violated
#' @description
#' Search within a data frame to find violations of Nelson Rule 1.
#' Nelson Rule 1: One point is more than 3 standard deviations from the mean.
#' @param df A data frame
#' @param dateCol A string denoting the date column
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
#'       ,aes(x = date
#'            ,y = measure)) +
#'  geom_line() +
#'  geom_hline(data = unique(df$ucl), yintercept = unique(df$ucl)) +
#'  geom_hline(data = unique(df$lcl), yintercept = unique(df$lcl)) +
#'  geom_hline(aes(yintercept = mean(df$measure)), linetype = 'dashed') +
#'  geom_point(data = df[df$violationFLG == TRUE,]
#'             ,aes(color = violationFLG))

nelsonRule1 <- function(df, measureCol, dateCol) {
  ucl <- mean(df[[measureCol]]) + 3 * sd(df[[measureCol]])
  lcl <- mean(df[[measureCol]]) - 3 * sd(df[[measureCol]])
  
  violationFLG <- df[[measureCol]] > ucl | df[[measureCol]] < lcl
  
  violationDSC <- ifelse(df[[measureCol]] > ucl, 'more than 3 standard deviations above the mean'
                         ,ifelse(df[[measureCol]] < lcl, 'more than 3 standard deviations below the mean',NA))
  
  df <- data.frame(date = df[[dateCol]]
                   ,measure = df[[measureCol]]
                   ,ucl = ucl
                   ,lcl = lcl
                   ,violationFLG = violationFLG
                   ,violationDSC = violationDSC)
  df
}
