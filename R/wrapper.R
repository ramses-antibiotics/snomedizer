

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
#' @param limit a positive integer for the maximum number of results to return.
#' See \code{\link{snomedizer_options}}. The maximum limit on public endpoints
#' is 10,000.
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
#' str(concept_find("asthma"))
#'
#' # Retrieve multiple concepts
#' concept_find(conceptIds =  c("233604007", "68566005"))
#'
#' # Use the SNOMED CT Expression Constraint Language
#' concept_find(
#'   ecl = paste(
#'     "<! 68566005 | Urinary tract infectious disease (disorder) |",
#'     "AND",
#'     "< 87628006 | Bacterial infectious disease (disorder) |"
#'     )
#'   )
concept_find <- function(term = NULL,
                         conceptIds = NULL,
                         ecl = NULL,
                         activeFilter = TRUE,
                         encoding = "UTF-8",
                         silent = FALSE,
                         limit = snomedizer_options_get("limit"),
                         ...) {

  CHUNK_SIZE = 100

  if(is.null(term) & is.null(conceptIds) & is.null(ecl)) {
    stop("At least `term` or `conceptIds` or `ecl` must be provided.")
  }

  if( !is.null(conceptIds) ) {
    conceptIds <- .snomed_identifiers_deduplicate(conceptIds)
  }

  if( !is.null(conceptIds) && length(conceptIds) > CHUNK_SIZE ) {

    if ( limit < CHUNK_SIZE ) {
      limit <- CHUNK_SIZE
    }

    progress_bar <- .progress_bar_initiate(x = conceptIds,
                                           chunk_size = CHUNK_SIZE,
                                           silent = silent)

    conceptIds <- .split_into_chunks(x = conceptIds,
                                     max_length = CHUNK_SIZE)

    x <- purrr::map(
      .x = conceptIds,
      .f = function(chunk,
                    term,
                    ecl,
                    activeFilter,
                    encoding,
                    progress_bar,
                    silent,
                    limit,
                    ...) {
        conc <- api_concepts(conceptIds = chunk, limit = limit, ...)
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
      progress_bar = progress_bar,
      silent = silent,
      limit = limit,
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
      limit = limit,
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


#' Determine whether one or more concepts belong to a target set of concepts
#'
#' @description This function compares a vector of \code{concept_ids} identifiers
#' against a target set defined by an ECL expression \code{target_ecl}.
#' It returns \code{TRUE} if the concept belongs to the target set,
#' \code{FALSE} if it does not, or
#' \code{NA} if it is not found in the branch.
#'
#' @param concept_ids character vector of identifiers of concepts to be analysed
#' @param target_ecl character ECL expression defining the target set of concepts.
#' The "<<" operator is required to include subtypes of a concept
#' @param silent whether to hide progress bar. Default is \code{FALSE}
#' @param endpoint URL of a SNOMED CT Terminology Server REST API endpoint.
#'  See \code{\link{snomedizer_options}}.
#' @param branch a string for the name of the API endpoint branch to use (most
#' commonly \code{"MAIN"}). See \code{\link{snomedizer_options}}.
#' @param encoding HTTP charset parameter to use (default is \code{"UTF-8"})
#' @return a logical vector with the same order as \code{concept_ids} indicating
#' whether each concept is included in the target set or not. \code{NA} is returned
#' when \code{concept_ids} are not present on the target \code{branch}, or in case
#' of a REST error caused by invalid \code{concept_ids} or \code{target_ecl}.
#' @export
#' @seealso \href{https://confluence.ihtsdotools.org/display/DOCECL/Appendix+D+-+ECL+Quick+reference}{ECL quick reference table by SNOMED International}
#' @examples
#' concept_included_in(
#'   concept_ids = "16227691000119107",  # Post-surgical excision site
#'   target_ecl = "<<123037004"          # Subtypes of 'body structure'
#' )
#' concept_included_in(
#'   concept_ids = "48800003",           # Ear lobule structure
#'   target_ecl = "<<233604007"          # Subtypes of 'pneumonia'
#' )
#' concept_included_in(
#'   concept_ids = "39732311000001104",  # Medical product only found UK Edition
#'   target_ecl = "27658006"             # Products containing amoxicillin
#' )
#' concept_included_in(
#'   concept_ids = "233604007",          # Pneumonia
#'   target_ecl = "<<64572001 :
#'          116676008 = <<409774005"     # Disorders with inflammation as associated morphology
#' )
#' concept_included_in(
#'   concept_ids = "233604007",          # Pneumonia
#'   target_ecl = "<<64572001 :
#'          116676008 = <<409774005"     # Disorders with inflammation as associated morphology
#' )
#' concept_included_in(
#'   concept_ids = "10625071000119104",  # Bronchopneumonia caused by bacteria (disorder) |
#'   target_ecl = "<<233604007 MINUS <<53084003"
#'                                       # Pneumonia excluding all bacterial pneumonia concepts
#' )
concept_included_in <- function(
  concept_ids,
  target_ecl,
  silent = FALSE,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  encoding = "UTF-8"
) {

  CHUNK_SIZE = 100

  stopifnot(length(target_ecl) == 1)

  unique_concept_ids <- .snomed_identifiers_deduplicate(concept_ids)

  if( length(unique_concept_ids) == 0 ) {
    return(rep(as.logical(NA), length(concept_ids)))
  }

  # First determine whether the input concepts are known
  valid_concepts <- concept_find(
    conceptIds = unique_concept_ids,
    silent = silent,
    endpoint = endpoint,
    branch = branch,
    encoding = encoding
  )

  if( is.null(valid_concepts) ) {
    return(rep(as.logical(NA), length(concept_ids)))
  }

  progress_bar <- .progress_bar_initiate(x = unique_concept_ids,
                                         chunk_size = CHUNK_SIZE,
                                         silent = silent)

  unique_concept_ids <- .split_into_chunks(x = unique_concept_ids,
                                           max_length = CHUNK_SIZE)

  x <- purrr::map(
    .x = unique_concept_ids,
    .f = function(chunk,
                  ecl,
                  endpoint,
                  branch,
                  encoding,
                  progress_bar,
                  silent,
                  CHUNK_SIZE) {
      output <- api_concepts(
        conceptIds = chunk,
        ecl = ecl,
        endpoint = endpoint,
        branch = branch,
        limit = CHUNK_SIZE)
      if ( !silent ) {
        progress_bar$tick()
      }

      if(httr::http_error(output)) {
        return(httr::content(output, encoding = encoding))
      } else if(length(httr::content(output, encoding = encoding)$items) == 0) {
        return(NULL)
      } else {
        ignore <- result_completeness(output)
        output <- result_flatten(output, encoding = encoding)
        return(output)
      }},
    ecl = target_ecl,
    endpoint = endpoint,
    branch = branch,
    encoding = encoding,
    progress_bar = progress_bar,
    silent = silent,
    CHUNK_SIZE = CHUNK_SIZE
  )

  x <- dplyr::bind_rows(x)

  if( nrow(x) == 0 ) {
    dplyr::case_when(
      concept_ids %in% valid_concepts$conceptId ~ FALSE,
      TRUE ~ as.logical(NA)
    )
  } else {
    dplyr::case_when(
      concept_ids %in% x$conceptId &
        concept_ids %in% valid_concepts$conceptId ~ TRUE,
      !(concept_ids %in% x$conceptId) &
        concept_ids %in% valid_concepts$conceptId ~ FALSE,
      TRUE ~ as.logical(NA)
    )
  }
}



#' Fetch active ancestors/descendants of one or more concepts
#'
#' @description Wrapper functions of \code{\link{api_concepts}} that fetch ancestors
#' or descendants of one or several concept identifiers using full SNOMED CT
#' inference.
#'
#' Note: these functions can only fetch active concepts.
#' @param conceptIds a character vector of concept identifiers
#' @param include_self a logical vector indicating whether the \code{conceptIds}
#' should be included in the results. The default is \code{TRUE}. If a single
#' value is provided, it will be recycled.
#' @param encoding HTTP charset parameter to use (default is \code{"UTF-8"})
#' @param silent whether to hide progress bar. Default is \code{FALSE}
#' @param ... other valid arguments to function \code{\link{api_concepts}},
#' for instance \code{endpoint}, \code{branch} or \code{limit}.
#' @return a named list of data frames
#' @family wrapper
#' @aliases concept_ancestors concept_descendants
#' @export
#' @section Disclaimer:
#' In order to use SNOMED CT, a licence is required which depends both on the country you are
#' based in, and the purpose of your work. See details on \link{snomedizer}.
#' @examples
#' pneumonia_ancestors <- concept_ancestors(conceptIds = "233604007")
#' # This will trigger a warning using the default limit set by snomedizer_options_get("limit")
#' pneumonia_concepts <- concept_descendants(conceptIds = "233604007")
#' # Raising the limit
#' pneumonia_concepts <- concept_descendants(conceptIds = "233604007", limit = 300)
#' head(pneumonia_concepts$`233604007`)
concept_ancestors <- function(conceptIds,
                                include_self = FALSE,
                                encoding = "UTF-8",
                                silent = FALSE,
                                ...) {

  .concept_xxscendants(conceptIds = conceptIds,
                        direction = "ancestors",
                        include_self = include_self,
                        encoding = encoding,
                        silent = silent,
                        ...)
}


#' @rdname concept_ancestors
#' @export
concept_descendants <- function(conceptIds,
                                 include_self = FALSE,
                                 encoding = "UTF-8",
                                 silent = FALSE,
                                 ...) {

  .concept_xxscendants(conceptIds = conceptIds,
                        direction = "descendants",
                        include_self = include_self,
                        encoding = encoding,
                        silent = silent,
                        ...)
}


#' Underlying function for concept_ancestors and concept_descendants
#' @noRd
.concept_xxscendants <- function(conceptIds,
                                  direction,
                                  include_self,
                                  encoding,
                                  silent,
                                  ...) {

  stopifnot(is.vector(conceptIds))
  stopifnot(all(!is.na(conceptIds)))
  if(!all(include_self %in% c(TRUE, FALSE))) {
    stop("`include_self` must be TRUE or FALSE.")
  }

  progress_bar <- .progress_bar_initiate(x = conceptIds,
                                         chunk_size = 1,
                                         silent = silent)

  x <- purrr::pmap(
    list(conceptIds,
         include_self),
    function(conceptId,
             include_self,
             direction,
             progress_bar,
             silent,...) {

      if (direction == "ancestors") {
        ecl <- paste0(dplyr::if_else(include_self, ">>", ">"), conceptId)
      } else {
        ecl <- paste0(dplyr::if_else(include_self, "<<", "<"), conceptId)
      }

      xxscendants <- api_concepts(
        ecl = ecl,
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
    },
    direction = direction,
    progress_bar = progress_bar,
    silent = silent,
    ...
  )

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
#' @param limit a positive integer for the maximum number of results to return.
#' See \code{\link{snomedizer_options}}. The maximum limit on public endpoints
#' is 10,000.
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
#' pneumonia_descriptions <- concept_descriptions(conceptIds = "233604007")
#' str(pneumonia_descriptions)
concept_descriptions <- function(conceptIds,
                                 encoding = "UTF-8",
                                 silent = FALSE,
                                 limit = snomedizer_options_get("limit"),
                                  ...) {
  CHUNK_SIZE = 100
  if ( length(conceptIds) > CHUNK_SIZE & limit < CHUNK_SIZE ) {
    limit <- 10000
  }

  stopifnot(is.vector(conceptIds))
  conceptIds <- .snomed_identifiers_deduplicate(conceptIds)
  stopifnot(length(conceptIds) > 0)

  progress_bar <- .progress_bar_initiate(x = conceptIds,
                                         chunk_size = CHUNK_SIZE,
                                         silent = silent)

  x <- .split_into_chunks(x = conceptIds, max_length = CHUNK_SIZE)

  x <- purrr::map(
    .x = x,
    .f = function(chunk, encoding, progress_bar, silent, limit,...) {
      desc <- api_descriptions(conceptIds = chunk, limit = limit, ...)
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
    progress_bar = progress_bar,
    silent = silent,
    limit = limit,
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
#' @param limit a positive integer for the maximum number of results to return.
#' See \code{\link{snomedizer_options}}. The maximum limit on public endpoints
#' is 10,000.
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
#' uti_concepts <- concept_map(target_code = "N39.0")
#' str(dplyr::select(uti_concepts,
#'                   referencedComponentId,
#'                   referencedComponent.pt.term,
#'                   additionalFields.mapTarget,
#'                   additionalFields.mapAdvice))
#'
#' # map SNOMED CT codes to ICD-10
#' map_icd10 <- concept_map(concept_ids = c("431308006", "312124009", "53084003"))
#' dplyr::select(map_icd10,
#'               referencedComponentId,
#'               referencedComponent.pt.term,
#'               additionalFields.mapTarget,
#'               additionalFields.mapAdvice)
concept_map <- function(concept_ids = NULL,
                        target_code = NULL,
                        map_refset_id = "447562003",
                        active = TRUE,
                        encoding = "UTF-8",
                        silent = FALSE,
                        limit = snomedizer_options_get("limit"),
                        ...) {

  CHUNK_SIZE = 100

  if( !is.null(concept_ids) ) {
    concept_ids <- .snomed_identifiers_deduplicate(concept_ids)
  }

  if( !is.null(concept_ids) && length(unique(concept_ids)) > CHUNK_SIZE ) {

    if ( limit < CHUNK_SIZE ) {
      limit <- 10000
    }

    progress_bar <- .progress_bar_initiate(x = concept_ids,
                                           chunk_size = CHUNK_SIZE,
                                           silent = silent)

    concept_ids <- .split_into_chunks(x = concept_ids,
                                      max_length = CHUNK_SIZE)

    x <- purrr::map(
      .x = concept_ids,
      .f = function(chunk,
                    referenceSet,
                    active,
                    mapTarget,
                    encoding,
                    progress_bar,
                    silent,
                    limit,
                    ...) {
        conc <- api_refset_members(
          referencedComponentId = chunk,
          referenceSet = referenceSet,
          active = active,
          mapTarget = mapTarget,
          limit = limit,
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
      progress_bar = progress_bar,
      silent = silent,
      limit = limit,
      ...
    )

    x <- dplyr::bind_rows(x)

  } else {
    x <- api_refset_members(
      referencedComponentId = concept_ids,
      referenceSet = map_refset_id,
      active = active,
      mapTarget = target_code,
      limit = limit,
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

  x
}

