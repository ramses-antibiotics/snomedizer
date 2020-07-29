

#' Sample of antibiotic prescription records
#'
#' @docType data
#' @description Dataset containing 403 records of free-text therapy indications
#' filled in by doctors when prescription antibiotics
#' @details A data frame with one row per therapy indication:
#' \describe{
#'    \item{\code{prescription_id}}{unique identifier for the record}
#'    \item{\code{indication}}{character vector of free-text therapy indication
#'    for the antibiotic prescription (eg \code{"urinary tract infection"})}
#' }
"drug_indications"
