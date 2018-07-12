#' Patient diabetes dataset
#'
#' A dataset containing diabetes status and other health-related variables for
#' 768 females, at least 21 years old, of Pima Indian heritage. As pointed out
#' (see source URL below), the source data had some biologically impossible zero
#' values. We have replaced zero values in every variable except
#' \code{Pregnancies} with NA.
#'
#' @format A tibble data frame with 768 rows and 10 variables:
#' \describe{
#'   \item{patient_id}{Unique identifier}
#'   \item{pregnancies}{Number of times pregnant}
#'   \item{plasma_glucose}{Plasma glucose concentration 2 hours in an oral
#'   glucose tolerance test}
#'   \item{diastolic_bp}{Diastolic blood pressure (mm Hg)}
#'   \item{skinfold}{Triceps skin fold thickness (mm)}
#'   \item{insulin}{2-Hour serum insulin (mu U/ml)}
#'   \item{weight_class}{Derived from BMI}
#'   \item{pedigree}{Diabetes pedigree function}
#'   \item{age}{Age (years)}
#'   \item{diabetes}{Y/N diagnosis per WHO criteria}
#' }
#'
#' @seealso \code{\link{pima_meds}}
#' @source \url{https://archive.ics.uci.edu/ml/datasets/pima+indians+diabetes}
"pima_diabetes"


#' Patient medications dataset
#'
#' This is a companion dataset for \code{\link{pima_diabetes}}. The
#' \code{pima_diabetes} dataset is real; this dataset is synthetic. You can see
#' how it was generated here:
#' \url{https://docs.healthcare.ai/articles/site_only/best_levels.html#appendix-data-generation}.
#' Briefly, each patient in \code{pima_diabetes} is assigned 0-4 medications
#' from the following six: insulin and metformin are more common among
#' diabetics, prednisone and metoprolol are less common among diabetics, and
#' nexium and tiotropium are equally likely among diabetic and non-diabetic
#' patients. Each patient-medication has a \code{years_taken} value associated
#' with it, which is a random number drawn from an exponential distribution.
#'
#' @format A tibble data frame with 1,604 rows and 3 variables:
#' \describe{
#'   \item{patient_id}{Unique identifier, used to join \code{pima_diabetes}}
#'   \item{medication}{One of the six medications described above}
#'   \item{years_taken}{Numeric value indicating the duration the medication has
#'   been used}
#' }
#' @seealso \code{\link{pima_diabetes}}
"pima_meds"
