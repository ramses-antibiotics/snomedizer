

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
#' @param encoding HTTP charset parameter to use (default is \code{"UTF-8"})
#' @param silent whether to hide progress bar. Default is \code{FALSE}
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
                          encoding = "UTF-8",
                          silent = FALSE,
                          ...) {


  if(is.null(term) & is.null(conceptIds) & is.null(ecl)) {
    stop("At least `term` or `conceptIds` or `ecl` must be provided.")
  }

  if( !is.null(conceptIds) ) {
    conceptIds <- sort(unique(conceptIds))
  }

  if( !is.null(conceptIds) && length(unique(conceptIds)) > 100 ) {

    if( !silent ) {
      progress_bar <- progress::progress_bar$new(
        format = "  [:bar] :percent :eta",
        total = (trunc(length(conceptIds)/100) + 1)
      )
      progress_bar$tick(0)
    }

    conceptIds <-  split(conceptIds, sort(trunc(seq_len(length(conceptIds))/100)))

    x <- purrr::map(
      .x = conceptIds,
      .f = function(chunk,
                    term,
                    ecl,
                    activeFilter,
                    encoding,
                    silent,
                    ...) {
        conc <- api_concepts(conceptIds = chunk, ...)
        if( !silent ) {
          progress_bar$tick()
        }
        if(httr::http_error(conc)) {
          return(httr::content(conc))
        } else if(length(httr::content(conc)$items) == 0) {
          return(NULL)
        } else {
          ignore <- result_completeness(conc)
          return(result_flatten(conc, encoding = encoding))
        }},
      term = term,
      ecl = ecl,
      activeFilter = activeFilter,
      encoding = encoding,
      silent = silent,
      ...
    )

    x <- dplyr::bind_rows(x)

    x

  } else {
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
      return(result_flatten(x, encoding = encoding))
    }
  }

  x
}


#' Fetch ascendants of one or more concepts
#'
#' @description This function is a wrapper of \code{\link{api_concepts}} that
#' fetches ascendants of one or several concept identifiers using full SNOMED CT
#' inference. The search can be restricted to parent concepts using the
#' \code{direct_ascendants} argument.
#' @param conceptIds a character vector of concept identifiers
#' @param direct_ascendants a logical vector indicating whether to fetch
#' direct ascendants (parents) of the \code{conceptIds} exclusively or all
#' ascendants (including parents). The default is \code{FALSE}. If a single
#' value is provided, it will be recycled.
#' @param activeFilter a logical vector indicating whether to fetch active
#' ascendant concepts exclusively. The default is \code{TRUE}. If a single
#' value is provided, it will be recycled.
#' @param encoding HTTP charset parameter to use (default is \code{"UTF-8"})
#' @param silent whether to hide progress bar. Default is \code{FALSE}
#' @param ... other valid arguments to function \code{\link{api_concepts}},
#' for instance \code{endpoint}, \code{branch} or \code{limit}.
#'
#' @return a named list of data frames
#' @family wrapper
#' @export
#' @section Disclaimer:
#' In order to use SNOMED CT, a licence is required which depends both on the country you are
#' based in, and the purpose of your work. See details on \link{snomedizer}.
#' @examples
#' # This will trigger a warning using the default limit set by snomedizer_options_get("limit")
#' pneumonia_concepts <- concepts_ascendants(conceptIds = "233604007")
concepts_ascendants <- function(conceptIds,
                                direct_ascendants = FALSE,
                                activeFilter = TRUE,
                                encoding = "UTF-8",
                                silent = FALSE,
                                ...) {

  stopifnot(all(direct_ascendants %in% c(TRUE, FALSE)))

  .concepts_xxscendants(conceptIds = conceptIds,
                        direction = "ascendants",
                        direct = direct_ascendants,
                        activeFilter = activeFilter,
                        encoding = encoding,
                        silent = silent,
                        ...)
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
#' @param encoding HTTP charset parameter to use (default is \code{"UTF-8"})
#' @param silent whether to hide progress bar. Default is \code{FALSE}
#' @param ... other valid arguments to function \code{\link{api_concepts}},
#' for instance \code{endpoint}, \code{branch} or \code{limit}.
#'
#' @return a named list of data frames
#' @family wrapper
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
                                 encoding = "UTF-8",
                                 silent = FALSE,
                                 ...) {

  stopifnot(all(direct_descendants %in% c(TRUE, FALSE)))

  .concepts_xxscendants(conceptIds = conceptIds,
                        direction = "descendants",
                        direct = direct_descendants,
                        activeFilter = activeFilter,
                        encoding = encoding,
                        silent = silent,
                        ...)
}


#' Underlying function for concepts_ascendants and concepts_descendants
#' @noRd
.concepts_xxscendants <- function(conceptIds,
                                  direction,
                                  direct,
                                  activeFilter,
                                  encoding,
                                  silent,
                                  ...) {

  stopifnot(is.vector(conceptIds))
  conceptIds <- sort(unique(conceptIds))
  stopifnot(is.null(activeFilter) || is.logical(activeFilter))

  if ( !silent ) {
    progress_bar <- progress::progress_bar$new(
      format = "  [:bar] :percent :eta",
      total = length(conceptIds)
    )
    progress_bar$tick(0)
  }

  if (direction == "ascendants") {
    ecl = paste0(dplyr::if_else(direct, ">!", ">"), conceptIds)
  } else {
    ecl = paste0(dplyr::if_else(direct, "<!", "<"), conceptIds)
  }

  x <- purrr::pmap(list(ecl, activeFilter),
                   function(ecl, activeFilter, silent, ...) {
                     xxscendants <- api_concepts(
                       ecl = ecl,
                       activeFilter = activeFilter,
                       ...)

                     if ( !silent ) {
                       progress_bar$tick()
                     }

                     if(httr::http_error(xxscendants)) {
                       return(httr::content(xxscendants))
                     } else if(length(httr::content(xxscendants)$items) == 0) {
                       return(NULL)
                     } else {
                       ignore <- result_completeness(xxscendants)
                       return(result_flatten(xxscendants, encoding = encoding))
                     }
                   }, silent = silent, ...)

  names(x) <- conceptIds

  x
}


#' Fetch descriptions of one or more concepts
#'
#' @description This function is a wrapper of \code{\link{api_descriptions}} that
#' fetches description of one or several concept identifiers.
#' @param conceptIds a character vector of concept identifiers
#' @param encoding HTTP charset parameter to use. Default is \code{"UTF-8"}.
#' @param silent whether to hide progress bar. Default is \code{FALSE}.
#' @param ... other optional arguments listed in \code{\link{api_operations}}, such as
#' \code{endpoint}, \code{branch} or \code{limit}
#' @return a named list of data frames sorted by \code{conceptIds}
#' @family wrapper
#' @export
#' @section Note:
#' Duplicate \code{conceptIds} will be removed.
#' @section Disclaimer:
#' In order to use SNOMED CT, a licence is required which depends both on the country you are
#' based in, and the purpose of your work. See details on \link{snomedizer}.
#' @examples
#' pneumonia_descriptions <- concepts_descriptions(conceptIds = "233604007")
#' str(pneumonia_descriptions)
concepts_descriptions <- function(conceptIds,
                                  encoding = "UTF-8",
                                  silent = FALSE,
                                  ...) {

  stopifnot(is.vector(conceptIds))
  stopifnot(all(conceptIds != ""))

  if( !silent ) {
    progress_bar <- progress::progress_bar$new(
      format = "  [:bar] :percent :eta",
      total = (trunc(length(conceptIds)/100) + 1)
    )
    progress_bar$tick(0)
  }

  stopifnot(all(conceptIds != ""))
  conceptIds <- sort(unique(conceptIds))

  x <-  split(conceptIds, sort(trunc(seq_len(length(conceptIds))/100)))

  x <- purrr::map(
    .x = x,
    .f = function(chunk, encoding, silent, ...) {
      desc <- api_descriptions(conceptIds = chunk, ...)
      if ( !silent ) {
        progress_bar$tick()
      }

      if(httr::http_error(desc)) {
        return(httr::content(desc))
      } else if(length(httr::content(desc)$items) == 0) {
        return(NULL)
      } else {
        ignore <- result_completeness(desc)
        return(result_flatten(desc, encoding = encoding))
      }},
    encoding = encoding,
    silent = silent,
    ...
  )

  x <- dplyr::bind_rows(x)
  x <- split(x, x$conceptId)

  x
}


#' Map SNOMED CT concepts to other terminology or code systems
#'
#' @description A wrapper function for the \code{\link{api_refset_members}()} function
#' to query Map Reference Sets, in particular the map to the World Health Organisation
#' International Classification of Diseases 10th Revision (ICD-10).
#' @param concept_ids an optional character vector of one or more SNOMED CT concept
#' identifiers to be mapped
#' @param target_code an optional character code designated the concept code in
#' the other terminology or code system
#' @param map_refset_id character identifier of a SNOMED CT Map Reference Set.
#' The default is \code{"447562003"} for the ICD-10 Map Reference Set.
#' @param active whether to restrict results to active concepts. Default is \code{TRUE}.
#' @param encoding HTTP charset parameter to use (default is \code{"UTF-8"})
#' @param silent whether to hide progress bar. Default is \code{FALSE}
#' @param ... other optional arguments listed in \code{\link{api_operations}}, such as
#' \code{endpoint}, \code{branch} or \code{limit}
#'
#' @return a data frame of SNOMED CT concepts mapped to another code system
#' @export
#'
#' @seealso World Health Organisation \href{https://icd.who.int/browse10/2016/en}{International Classification of Diseases 10th Revision}
#' @seealso SNOMED International \href{https://confluence.ihtsdotools.org/display/DOCRELFMT/5.2.10+Complex+and+Extended+Map+Reference+Sets}{Map Reference Sets}
#' @seealso SNOMED International \href{http://snomed.org/icd10map}{ICD-10 Mapping Technical Guide}
#' @examples
#' # find SNOMED CT codes corresponding to ICD-10 code N39.0 urinary tract infections
#' uti_concepts <- concepts_map(target_code = "N39.0")
#' str(dplyr::select(uti_concepts,
#'                   referencedComponentId,
#'                   referencedComponent.pt.term,
#'                   additionalFields.mapTarget,
#'                   additionalFields.mapAdvice))
#'
#' # map SNOMED CT codes to ICD-10
#' map_icd10 <- concepts_map(concept_ids = c("431308006", "312124009", "53084003"))
#' dplyr::select(map_icd10,
#'               referencedComponentId,
#'               referencedComponent.pt.term,
#'               additionalFields.mapTarget,
#'               additionalFields.mapAdvice)
concepts_map <- function(concept_ids = NULL,
                         target_code = NULL,
                         map_refset_id = "447562003",
                         active = TRUE,
                         encoding = "UTF-8",
                         silent = FALSE,
                         ...) {

  if( !is.null(concept_ids) ) {
    concept_ids <- sort(unique(concept_ids))
  }

  if( !is.null(concept_ids) && length(unique(concept_ids)) > 100 ) {

    if( !silent ) {
      progress_bar <- progress::progress_bar$new(
        format = "  [:bar] :percent :eta",
        total = (trunc(length(concept_ids)/100) + 1)
      )
      progress_bar$tick(0)
    }

    concept_ids <- split(concept_ids, sort(trunc(seq_len(length(concept_ids))/100)))

    x <- purrr::map(
      .x = concept_ids,
      .f = function(chunk,
                    referenceSet,
                    active,
                    mapTarget,
                    encoding,
                    silent,
                    ...) {
        conc <- api_refset_members(
          referencedComponentId = chunk,
          referenceSet = referenceSet,
          active = active,
          mapTarget = mapTarget,
          ...
        )
        if( !silent ) {
          progress_bar$tick()
        }
        if(httr::http_error(conc)) {
          return(httr::content(conc))
        } else if(length(httr::content(conc)$items) == 0) {
          return(NULL)
        } else {
          ignore <- result_completeness(conc)
          concepts <- httr::content(conc, encoding = encoding)
          concepts <- lapply(concepts[["items"]], as.data.frame)
          concepts <- dplyr::bind_rows(concepts)
          return(concepts)
        }},
      referenceSet = map_refset_id,
      active = active,
      mapTarget = target_code,
      encoding = encoding,
      silent = silent,
      ...
    )

    x <- dplyr::bind_rows(x)

    return(x)

  } else {
    x <- api_refset_members(
      referencedComponentId = concept_ids,
      referenceSet = map_refset_id,
      active = active,
      mapTarget = target_code,
      ...
    )

    if(httr::http_error(x)) {
      return(httr::content(x))
    } else if(length(httr::content(x)$items) == 0) {
      return(NULL)
    } else {
      ignore <- result_completeness(x)
      concepts <- httr::content(x, encoding = encoding)
      concepts <- lapply(concepts[["items"]], as.data.frame)
      concepts <- dplyr::bind_rows(concepts)
      return(concepts)
    }
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
#' @family wrapper
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
  )[[1]]

  ct_version <- dplyr::filter(ct_version,
                              active == TRUE,
                              grepl("version:", term))

  list(
    "rf2_date" = regmatches(
      ct_version$term,
      regexpr("[0-9]{8}", ct_version$term)
    ),
    "rf2_month_year" = regmatches(
      ct_version$term,
      regexpr("(?<=[(])(.*)(?= Release[)]$)",
              ct_version$term, perl = T)
    )
  )
}
