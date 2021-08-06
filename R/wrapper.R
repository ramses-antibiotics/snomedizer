

#' Find SNOMED CT concepts
#'
#' @description A wrapper function for \code{\link{api_concepts}} searching for a
#' term or a list of concepts
#' @param term a string containing search terms
#' @param conceptIds a character vector of one or more SNOMED CT codes
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
#' @section Disclaimer:
#' In order to use SNOMED CT, a licence is required which depends both on the country you are
#' based in, and the purpose of your work. See details on \link{snomedizer}.
#' @export
#' @family wrapper
#' @examples
#' # Free text search
#' str(concepts_find("asthma"))
#'
#' # Retrieve multiple concepts
#' concepts_find(conceptIds =  c("233604007", "68566005"))
#'
#' # Use the SNOMED CT Expression Constraint Language
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

  if(httr::http_error(x)) {
    return(httr::content(x))
  } else if(length(httr::content(x)$items) == 0) {
    return(NULL)
  } else {
    ignore <- result_completeness(x)
    return(result_flatten(x))
  }
}


#' Fetch descendants of one or more concepts
#'
#' @description This function is a wrapper of \code{\link{api_concepts}} that
#' fetches descendants of one or several concept identifiers using full SNOMED CT
#' inference. The search can be restricted to children concepts using the
#' \code{direct_descendants} argument.
#' @param conceptIds a character vector of concept identifiers
#' @param direct_descendants a logical vector indicating whether to fetch
#' direct descendants (children) of the \code{conceptIds} exclusively or all
#' descendants (including children). The default is \code{FALSE}. If a single
#' value is provided, it will be recycled.
#' @param activeFilter a logical vector indicating whether to fetch active
#' descendant concepts exclusively. The default is \code{TRUE}. If a single
#' value is provided, it will be recycled.
#' @param ... other valid arguments to function \code{\link{api_concepts}},
#' for instance \code{endpoint}, \code{branch} or \code{limit}.
#'
#' @return a named list of data frames
#' @export
#' @section Disclaimer:
#' In order to use SNOMED CT, a licence is required which depends both on the country you are
#' based in, and the purpose of your work. See details on \link{snomedizer}.
#' @examples
#' # This will trigger a warning using the default limit set by snomedizer_options_get("limit")
#' pneumonia_concepts <- concepts_descendants(conceptIds = "233604007")
#' # Raising the limit
#' pneumonia_concepts <- concepts_descendants(conceptIds = "233604007", limit = 300)
#' head(pneumonia_concepts$`233604007`)
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

    progress_bar$tick()$print()

    if(httr::http_error(descendants)) {
      return(httr::content(descendants))
    } else if(length(httr::content(descendants)$items) == 0) {
      return(NULL)
    } else {
      ignore <- result_completeness(descendants)
      return(result_flatten(descendants))
    }
  }, ...)
  names(x) <- conceptIds

  x
}


#' Fetch descriptions of one or more concepts
#'
#' @description This function is a wrapper of \code{\link{api_descriptions}} that
#' fetches description of one or several concept identifiers.
#' @param conceptIds a character vector of concept identifiers
#' @param ... other optional arguments listed in \code{\link{api_operations}}, such as
#' \code{endpoint}, \code{branch} or \code{limit}
#' @return a named list of data frames
#' @export
#' @section Disclaimer:
#' In order to use SNOMED CT, a licence is required which depends both on the country you are
#' based in, and the purpose of your work. See details on \link{snomedizer}.
#' @examples
#' pneumonia_descriptions <- concepts_descriptions(conceptIds = "233604007")
#' str(pneumonia_descriptions)
concepts_descriptions <- function(conceptIds, ...) {

  stopifnot(is.vector(conceptIds))
  stopifnot(all(conceptIds != ""))
  desc <- api_descriptions(conceptIds = conceptIds, ...)

  if(httr::http_error(desc)) {
    return(httr::content(desc))
  } else if(length(httr::content(desc)$items) == 0) {
    return(NULL)
  } else {
    ignore <- result_completeness(desc)
    return(result_flatten(desc))
  }
}


#' Fetch SNOMED CT RF2 release version
#'
#' @description Provides the date of the release of the specified endpoint
#' and branch. SNOMED CT is currently released released twice a year
#' on 31 January and 31 July in a format known as Release Format 2 (RF2).
#' @param endpoint the URL of a SNOMED CT Terminology Server REST API endpoint.
#'  See \code{\link{snomedizer_options}}.
#' @param branch a string for the name of the API endpoint branch to use (most
#' commonly \code{"MAIN"}). See \code{\link{snomedizer_options}}.
#' @return a list containing two character strings: \code{rf2_date}
#' (YYYYMMDD release date) and \code{rf2_month_year} (month and year string)
#' @references \href{SNOMED CT Release File Specifications}{http://snomed.org/rfs}
#' @export
release_version <- function(endpoint = snomedizer_options_get("endpoint"),
                            branch = snomedizer_options_get("branch")) {

  active <- term <- NULL

  ct_version <- concepts_descriptions(
    conceptIds = "138875005",
    endpoint = endpoint,
    branch = branch,
    limit = 1000
  )

  ct_version <- dplyr::filter(ct_version,
                              active == TRUE,
                              grepl("version:", term))

  list(
    rf2_date = stringr::str_extract(ct_version$term, "[0-9]{8}"),
    rf2_month_year = stringr::str_match(ct_version$term, "[(](.*) Release[)]$")[,2]
  )
}

#' #' Get all SNOMED CT infection concepts
#' #'
#' #' @description Obtain all concepts belonging to the
#' #' \code{40733004 |Infectious disease (disorder)|}
#' #' SNOMED CT concept together with the preferred name,
#' #' @param limit a positive integer for the maximum number of results to return.
#' #' See \code{\link{snomedizer_options}}. The maximum limit on public endpoints
#' #' is 10,000.
#' #' @param ... optional arguments listed in \code{\link{api_operations}}, such as
#' #' \code{endpoint}, \code{branch} or \code{limit}
#' #'
#' #' @return A data frame containing the following variables:
#' #'     \itemize{
#' #'       \item[conceptId] a character vector of SNOMED CT conceptIds
#' #'       \item[term] a character vector of fully specified names
#' #'       \item[causalAgent] a factor characterising the pathogen causing the infection:
#' #'           bacterial, fungal, viral, unspecified
#' #'       \item[onset] a factor characterising the onset of disease: community or healthcare
#' #'       \item[class1] a character vector containing the conceptId of
#' #'          a parent SNOMED CT concept.
#' #'     }
#' #' @export
#' #'
#' #' @examples
#' #'    # To get all 6,769 infection codes in SNOMED CT UK (as of December 2019)
#' #'    inf_concepts <- concept_infections(limit = 7000)
#' #'    str(inf_concepts)
#' concept_infections <- function(limit = 9000, ...) {
#'   concepts_descendants(conceptIds = "40733004",
#'                        direct_descendants = FALSE,
#'                        limit = limit,
#'                        ...)
#' }

