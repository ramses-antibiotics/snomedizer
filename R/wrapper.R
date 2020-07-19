

#' Find SNOMED-CT concepts
#'
#' @description A wrapper function searching for a term or a list of
#' concepts
#' @param term a string containing search terms
#' @param conceptIds a character vector of one or more SNOMED-CT codes
#' to search
#' @param ecl a character expression contraint. Consult the
#' \href{http://snomed.org/ecl}{Expression Constraint Language guide}
#' for more detail.
#' @param activeFilter whether to restrict results to active concepts. Default is `TRUE`.
#' Consult the \href{http://snomed.org/gl}{SNOMED glossary} for more detail.
#' @param silent whether to hide warnings. Default is `FALSE`
#' @param ... other optional arguments listed in \code{\link{api_operations}}
#' @return a data frame
#' @export
#' @family wrapper
#' @examples
#' # Free text search
#' concepts_find("asthma")
#'
#' # Retrieve multiple concepts
#' concepts_find(conceptIds =  c("233604007", "68566005"))
#'
#' # Use the SNOMED-CT Expression Constraint Language
#' concepts_find(
#'   ecl = paste(
#'     "<! 68566005 | Urinary tract infectious disease (disorder) |",
#'     "AND",
#'     "< 87628006 | Bacterial infectious disease (disorder) |"
#'     )
#'   )
concepts_find <- function(term = NULL,
                          conceptIds = NULL,
                          ecl = NULL,
                          activeFilter = TRUE,
                          silent = FALSE,
                          ...) {


  if(is.null(term) & is.null(conceptIds) & is.null(ecl)) {
    stop("At least `term` or `conceptIds` or `ecl` must be provided.")
  }

  x <- api_concepts(
    term = term,
    conceptIds = conceptIds,
    ecl = ecl,
    activeFilter = activeFilter,
    ...
  )
  ignore <- result_completeness(x, silent = silent)

  result_flatten(x)
}


#' #' Get all SNOMED-CT infection concepts
#' #'
#' #' @description Obtain all concepts belonging to the \rm{40733004 |Infectious disease (disorder)|}
#' #'     SNOMED-CT concept together with the preferred name,
#' #'
#' #' @param limit an integer standing for the maximum number of results
#' #'     to get. Default is set in \code{\link{default.snowstorm.limit}}
#' #'
#' #' @return A data frame containing the following variables:
#' #'     \itemize{
#' #'       \item[conceptId] a character vector of SNOMED-CT conceptIds
#' #'       \item[term] a character vector of fully specified names
#' #'       \item[causalAgent] a factor characterising the pathogen causing the infection:
#' #'           bacterial, fungal, viral, unspecified
#' #'       \item[onset] a factor characterising the onset of disease: community or healthcare
#' #'       \item[class1] a character vector containing the conceptId of
#' #'          a parent SNOMED-CT concept.
#' #'     }
#' #' @export
#' #'
#' #' @examples
#' #'    # To get all 6,769 infection codes in SNOMED-CT UK (as of December 2019)
#' #'    inf_concepts <- snowstorm_fetch_infections(limit = 7000)
#' #'    str(inf_concepts)
#' snowstorm_fetch_infections <- function(limit = getOption("snowstorm.limit")) {
#'   snowstorm_fetch_children("40733004", direct = F, limit = limit)
#' }
#'
#'
