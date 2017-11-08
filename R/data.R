#' Patient diabetes dataset
#'
#' A dataset containing diabetes status and other health-related variables
#' for 768 females, at least 21 years old, of Pima Indian heritage. There are no
#' missing values in this dataset. However, as pointed in the source (link 
#' below), note that some zero values are biologically impossible and likely
#' represent missingness.
#'
#' @format A tibble data frame with 768 rows and 10 variables:
#' \describe{
#'   \item{PatientID}{unique identifier}
#'   \item{Pregnancies}{Number of times pregnant}
#'   \item{PlasmaGlucose}{Plasma glucose concentration 2 hours in an oral 
#'   glucose tolerance test}
#'   \item{DiastolicBP}{Diastolic blood pressure (mm Hg)}
#'   \item{TricepSkinfoldThickness}{Triceps skin fold thickness (mm)}
#'   \item{Insulin}{2-Hour serum insulin (mu U/ml)}
#'   \item{BMI}{Body mass index (weight in kg/(height in m)^2)}
#'   \item{DiabetesPedigreeFunction}{Diabetes pedigree function}
#'   \item{Age}{Age (years)}
#'   \item{Diabetes}{Y/N diagnosis per WHO criteria}
#' }
#' 
#' @source \url{https://archive.ics.uci.edu/ml/datasets/pima+indians+diabetes}
"pima_diabetes"
