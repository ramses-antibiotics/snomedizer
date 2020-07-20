

#' Find SNOMED-CT concepts
#'
#' @description A wrapper function for \code{\link{api_concepts}} searching for a
#' term or a list of concepts
#' @param term a string containing search terms
#' @param conceptIds a character vector of one or more SNOMED-CT codes
#' to search
#' @param ecl a character expression constraint query (with full inference). Consult the
#' \href{http://snomed.org/ecl}{Expression Constraint Language guide}
#' for more detail.
#' @param activeFilter whether to restrict results to active concepts. Default is `TRUE`.
#' Consult the \href{http://snomed.org/gl}{SNOMED glossary} for more detail.
#' @param silent whether to hide warnings. Default is `FALSE`
#' @param ... other optional arguments listed in \code{\link{api_operations}}, such as
#' \code{endpoint}, \code{branch} or \code{limit}
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


#' Fetch descendants of one or more concepts
#'
#' @description This function is a wrapper of \code{\link{api_concepts}} that
#' fetches descendants of one or several concept identifiers using full SNOMED-CT
#' inference. The search can be restricted to children concepts using the
#' \code{direct_descendants} argument.
#' @param conceptIds a character vector of concept identifiers
#' @param direct_descendants a logical vector indicating whether to fetch
#' direct descendants (children) of the \code{conceptIds} exclusively or all
#' descendants. The default is \code{FALSE}. If a single
#' value is provided, it will be recycled.
#' @param activeFilter a logical vector indicating whether to fetch active
#' descendant concepts exclusively. The default is \code{TRUE}. If a single
#' value is provided, it will be recycled.
#' @param ... other valid arguments to function \code{\link{api_concepts}},
#' for instance \code{endpoint}, \code{branch} or \code{limit}.
#'
#' @return a named list of data frames
#' @export
#'
#' @examples
#' # This will trigger a warning using the default limit set by snomedizer_options_get("limit")
#' concepts_descendants(conceptIds = c("233604007", "68566005"))
#' # Raising the limit
#' concepts_descendants(conceptIds = c("233604007", "68566005"), limit = 300)
concepts_descendants <- function(conceptIds,
                                 direct_descendants = FALSE,
                                 activeFilter = TRUE,
                                 ...) {

  stopifnot(is.vector(conceptIds))
  stopifnot(all(direct_descendants %in% c(TRUE, FALSE)))

  progress_bar <- dplyr::progress_estimated(length(conceptIds))

  ecl = paste0(dplyr::if_else(direct_descendants, "<!", "<"), conceptIds)

  x <- purrr::pmap(list(ecl, activeFilter),
                    function(ecl, activeFilter, ...) {
    descendants <- api_concepts(
      ecl = ecl,
      activeFilter = activeFilter,
      ...)
    ignore <- result_completeness(descendants)
    progress_bar$tick()$print()

    result_flatten(descendants)
  }, ...)
  names(x) <- conceptIds

  x
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
