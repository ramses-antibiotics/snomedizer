
#' SNOMED CT Terminology Server REST API operations
#'
#' @description Low-level wrapper functions based on \link{httr} for interfacing
#' with the operations built in the \href{https://github.com/IHTSDO/snowstorm}{Snowstorm API}
#' @param acceptableIn character vector of description language reference sets
#' (example: \code{"900000000000509007"}).
#' The description must be acceptable in at least one of these to match.
#' @param active optional boolean: \itemize{
#'     \item \code{TRUE} returns only active terminology
#'     \item \code{FALSE} returns only inactive terminology
#'     \item \code{NULL} (the default) returns both active and inactive terminology
#' }
#' @param activeFilter optional boolean: \itemize{
#'     \item \code{TRUE} returns only active terminology
#'     \item \code{FALSE} returns only inactive terminology
#'     \item \code{NULL} (the default) returns both active and inactive terminology
#' }
#' @param branch a string for the name of the API endpoint branch to use (most
#' commonly \code{"MAIN"}). See \code{\link{snomedizer_options}}.
#' @param catch404 whether to display a warning if the API operation returns a
#' '404 Not Found' status. Default is \code{TRUE}.
#' @param characteristicType a character string indicating whether to include
#' results for: \itemize{
#'     \item all relationships: NULL (the default)
#'     \item only stated relationships: "STATED_RELATIONSHIP"
#'     \item only inferred relationships: "INFERRED_RELATIONSHIP"
#'     \item only additional relationships: ""ADDITIONAL_RELATIONSHIP" (for
#'     instance, \code{123005000 | Part of (attribute) |})
#' }
#' This parameter corresponds to \code{
#' 900000000000449001 | Characteristic type (core metadata concept)}
#' @param conceptId character string of a SNOMED CT concept identifier (for example:
#' \code{"233604007"})
#' @param conceptIds a character vector of SNOMED CT concept identifiers (for example:
#' \code{c("233604007", "68566005")})
#' @param conceptActive optional boolean: \itemize{
#'     \item \code{TRUE} returns only active concepts
#'     \item \code{FALSE} returns only inactive concepts
#'     \item \code{NULL} (the default) returns both active and inactive concepts
#' }
#' @param conceptRefset character vector of reference sets concept identifiers
#' to include (example: \code{"900000000000497000"} for CTV3 terminology).
#' See \code{api_concept_descendants("900000000000455006")}
#' for valid reference set concepts.
#' @param descendantCountForm a character string indicating whether to report
#' the count of descendant concepts based on stated or inferred relationships.
#' Must be one of \code{"inferred"}, \code{"stated"}, or \code{"additional"}.
#' Default is \code{NULL} for no descendant count reported.
#' @param destination concept character string restricting the range of the
#' relationships to be included in results
#' @param ecl a character expression constraint query (with full relationship inference).
#' Consult the \href{http://snomed.org/ecl}{Expression Constraint Language guide}
#' for more detail.
#' @param eclStated a character expression constraint query (limited to stated relationships).
#' Consult the \href{http://snomed.org/ecl}{Expression Constraint Language guide}
#' for more detail.
#' @param endpoint URL of a SNOMED CT Terminology Server REST API endpoint.
#'  See \code{\link{snomedizer_options}}.
#' @param form a character string indicating which ancestors/parents or
#' descendants/children to extract based on stated or inferred relationships.
#' Must be one of \code{"inferred"} (default), \code{"stated"}, or \code{"additional"}.
#' @param forBranch a character name of a single branch (eg \code{"MAIN"}) for which
#' to fetch code systems results. The default (\code{NULL}) will return all code systems.
#' @param groupByConcept a boolean indicating whether to group descriptions
#' by concept. Default is \code{FALSE}.
#' @param includeDescendantCount a boolean indicating whether a number of
#' children/descendants counter should be included in the result
#' @param language vector of two-character language codes to include
#' (example: \code{c("en", "de")}).
#' @param limit a positive integer for the maximum number of results to return.
#' See \code{\link{snomedizer_options}}. The maximum limit on public endpoints
#' is 10,000.
#' @param mapTarget target code to which the SNOMED CT concept represented the
#' \code{referencedComponentId} is mapped in the target code system, classification,
#' or terminology (eg ICD-10). This is only used for Map Reference Sets
#' @param module character vector of SNOMED CT modules to include (example:
#' \code{"900000000000207008"})
#' @param offset an integer indicating the number of results to skip
#' @param owlExpression.conceptId a string for a concept identifier within an
#' owlExpression. Consult the
#' \href{http://snomed.org/owl}{SNOMED CT OWL Guide} for detail.
#' @param owlExpression.gci a boolean indicating whether to return axiom members
#' with a GCI owlExpression (\code{TRUE}), without (\code{FALSE}), or all members
#' (\code{NULL}, the default). Consult the
#' \href{http://snomed.org/owl}{SNOMED CT OWL Guide} for detail.
#' @param preferredIn character vector of description language reference sets
#' (example: \code{"900000000000509007"}).
#' The description must be preferred in at least one of these to match.
#' @param preferredOrAcceptableIn character vector of description language reference sets
#' (example: \code{"900000000000509007"}).
#' The description must be preferred OR acceptable in at least one of these to match.
#' @param referenceSet a string for a reference set identifier or ECL expression
#' can be used to limit the reference sets searched. Example: \code{"<723564002"}
#' @param referenceSetModule a string identifier for a SNOMED CT module containing
#' the reference sets to include. An ECL expression can be used to limit
#' the modules searched, for example: \code{"<900000000000445007"}
#' @param referencedComponentId a character vector of identifiers of
#' SNOMED CT components to be included. For Map Reference Sets, this refers
#' to the SNOMED CT concept that is mapped to the other terminology or code system
#' @param relationshipId string of a relationship concept
#' @param searchAfter integer for the number of results to skip. May be used for
#' for querying more that 10,000 records (current \code{limit} on results returned)
#' @param searchMode a character string for the search mode. Must be either
#' \code{"STANDARD"} (default) or \code{"REGEX"}.
#' @param semanticTag character string of a description semantic tag
#' to include (example: \code{"attribute"}). See
#' \code{api_descriptions_semantic_tags()} for a list of valid
#' description semantic tags.
#' @param semanticTags character vector of description semantic tags
#' to include (example: \code{c("attribute", "finding")}). See
#' \code{api_descriptions_semantic_tags()} for a list of valid
#' description semantic tags.
#' @param shortName character name of a code system (eg \code{"SNOMEDCT"},
#' \code{"SNOMEDCT-UK"})
#' @param showFutureVersions a boolean indicating whether to include all code
#' systems (\code{NULL}), only future code systems (\code{TRUE}),
#' or no future code systems (\code{FALSE}, the default)
#' @param showInternalReleases a boolean indicating whether to include all
#' terminology releases (\code{NULL}), only internal releases (\code{TRUE}), or
#' only external releases (\code{FALSE}, the default)
#' @param source a character vector of concepts to be included as
#' sources defined by the relationship
#' @param stated a boolean indicating whether to limit search to descendants
#' whose relationship is stated rather than inferred. Default is \code{FALSE}.
#' @param targetComponent string identifier the target code
#' (concept or description) in an Association Reference Set. Consult the
#' \href{https://confluence.ihtsdotools.org/display/DOCRELFMT/5.2.5+Association+Reference+Set}{Association Reference Set data structure}
#' for detail.
#' @param term character vector of terms to search
#' @param type character vector of concept codes defining the type of description or
#' the type of attribute/relationship to include, depending on the function:
#' \itemize{
#'    \item see \code{api_concept_descendants("900000000000446008")} for valid
#'    description type concepts.
#'    \item see \code{api_concept_descendants("106237007")} for valid
#'    attributes (relationship types) concepts.
#'  }
#' @param ... other REST API parameters
#' @importFrom httr parse_url build_url GET
#' @return An \code{httr} \code{\link[httr]{response}()} object.
#' @name api_operations
#' @family api_operations
#' @section Disclaimer:
#' In order to use SNOMED CT, a licence is required which depends both on the country you are
#' based in, and the purpose of your work. See details on \link{snomedizer}.
#' @examples
#' # look up the pneumonia concept
#' api_concept(conceptId = "233604007")
#' api_concepts(term = "pneumonia")
#' api_concepts(conceptIds = c("233604007", "68566005"))
#'
#' # get the content of the server request
#' pneumonia <- httr::content(api_concepts(term = "pneumonia"), limit = 1)
#' str(pneumonia$items[[1]])
NULL

#' @rdname api_operations
#' @export
api_concept <- function(conceptId,
                        endpoint = snomedizer_options_get("endpoint"),
                        branch = snomedizer_options_get("branch"),
                        catch404 = TRUE) {
  # GET /{branch}/concepts/{conceptId}

  stopifnot(is.character(conceptId))
  stopifnot(length(conceptId) == 1)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "concepts",
                     conceptId)
  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404) {
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_concepts <- function(
  term = NULL,
  conceptIds = NULL,
  ecl = NULL,
  eclStated = NULL,
  activeFilter = NULL,
  searchAfter = NULL,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  limit = snomedizer_options_get("limit"),
  offset = 0,
  catch404 = TRUE,
  ...
) {
  # GET /{branch}/concepts

  stopifnot(is.vector(conceptIds) | is.null(conceptIds))
  stopifnot(length(term) == 1 | is.null(term))
  stopifnot(length(ecl) == 1 | is.null(ecl))
  stopifnot(length(eclStated) == 1 | is.null(eclStated))

  conceptIds <- .concatenate_array_parameter(conceptIds)
  stopifnot(is.null(offset) | length(offset) == 1)
  limit <- .validate_limit(limit)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "concepts")
  rest_url$query <- list(
    term = term,
    conceptIds = conceptIds,
    ecl = ecl,
    eclStated = eclStated,
    limit = limit,
    offset = offset,
    activeFilter = activeFilter,
    searchAfter = searchAfter
  )
  rest_url$query <- append(rest_url$query, list(...))
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_concept_descendants <- function(
  conceptId,
  stated = FALSE,
  limit = snomedizer_options_get("limit"),
  offset = 0,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE
) {
  # GET /{branch}/concepts/{conceptId}/descendants

  stopifnot(is.character(conceptId))
  stopifnot(is.null(offset) | length(offset) == 1)
  stopifnot(length(conceptId) == 1)
  stopifnot(stated %in% c(TRUE, FALSE))
  limit <- .validate_limit(limit)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "concepts",
                     conceptId,
                     "descendants")
  rest_url$query <- list(
    stated = stated,
    limit = limit,
    offset = offset
  )
  rest_url <- httr::build_url(rest_url)

  rest_result <- GET(rest_url)

  if(catch404) {
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_concept_descriptions <- function(
  conceptId,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE
) {
  # GET /{branch}/concepts/{conceptId}/descriptions

  stopifnot(is.character(conceptId))
  stopifnot(length(conceptId) == 1)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "concepts",
                     conceptId,
                     "descriptions")
  rest_url <- httr::build_url(rest_url)

  rest_result <- GET(rest_url)

  if(catch404) {
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_all_branches <- function(endpoint = snomedizer_options_get("endpoint"),
                             catch404 = TRUE) {
  # GET /branches

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "branches")
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_branch <- function(endpoint = snomedizer_options_get("endpoint"),
                       branch = snomedizer_options_get("branch"),
                       catch404 = TRUE,
                       ...) {
  # GET /branches/{path}

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "branches",
                     branch)
  rest_url$query <- list(...)
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_branch_descendants <- function(
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE,
  ...) {
  # GET /branches/{path}/children

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "branches",
                     branch,
                     "children")
  rest_url$query <- list(...)
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_descriptions <- function(
  conceptIds = NULL,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  offset = 0,
  limit = snomedizer_options_get("limit"),
  catch404 = TRUE,
  ...) {
  # GET /{branch}/descriptions

  stopifnot(is.character(conceptIds))
  conceptIds <- .concatenate_array_parameter(conceptIds)
  limit <- .validate_limit(limit)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "descriptions")
  rest_url$query <- list(
    conceptIds = conceptIds,
    offset = offset,
    limit = limit
  )
  rest_url$query <- append(rest_url$query, list(...))
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_version <- function(
  endpoint = snomedizer_options_get("endpoint"),
  catch404 = TRUE) {
  # GET /version

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "version")

  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_browser_concepts <- function(
    conceptId,
    descendantCountForm = c(NULL, "inferred", "stated", "additional"),
    endpoint = snomedizer_options_get("endpoint"),
    branch = snomedizer_options_get("branch"),
    catch404 = TRUE
  ) {
  # GET /browser/{branch}/concepts/{conceptId}

  stopifnot(is.character(conceptId))
  stopifnot(length(conceptId) == 1)
  descendantCountForm <- descendantCountForm[1]
  if(!is.null(descendantCountForm)) {
    stopifnot(descendantCountForm %in% c("inferred", "stated", "additional"))
  }

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "browser",
                     branch,
                     "concepts",
                     conceptId)
  rest_url$query <- list(
    descendantCountForm = descendantCountForm
  )
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_browser_concept_ancestors <- function(
  conceptId,
  form = "inferred",
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE
) {
  # GET /browser/{branch}/concepts/{conceptId}/ancestors

  stopifnot(is.character(conceptId))
  stopifnot(length(conceptId) == 1)
  stopifnot(!is.null(form))
  stopifnot(length(form) == 1)
  stopifnot(!is.na(form))
  stopifnot(form %in% c("inferred", "stated", "additional"))

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "browser",
                     branch,
                     "concepts",
                     conceptId,
                     "ancestors")
  rest_url$query <- list(
    form = form
  )
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_browser_concept_children <- function(
  conceptId,
  form = "inferred",
  includeDescendantCount = TRUE,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE
) {
  # GET /browser/{branch}/concepts/{conceptId}/children

  stopifnot(is.character(conceptId))
  stopifnot(length(conceptId) == 1)
  stopifnot(!is.null(form))
  stopifnot(length(form) == 1)
  stopifnot(!is.na(form))
  stopifnot(form %in% c("inferred", "stated", "additional"))

  stopifnot(!is.null(includeDescendantCount),
            is.logical(includeDescendantCount) &
              !is.na(includeDescendantCount))

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "browser",
                     branch,
                     "concepts",
                     conceptId,
                     "children")
  rest_url$query <- list(
    form = form,
    includeDescendantCount = includeDescendantCount
  )
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}


#' @rdname api_operations
#' @export
api_browser_concept_parents <- function(
  conceptId,
  form = "inferred",
  includeDescendantCount = TRUE,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE
) {
  # GET /browser/{branch}/concepts/{conceptId}/parents

  # Note: Unlike api_browser_concept_children, this only provides
  # isleafInferred flag if includeDescendantCount = TRUE
  # https://github.com/IHTSDO/snowstorm/blob/07e3f7ea08f8091b20f856a4775f867608961329/src/main/java/org/snomed/snowstorm/rest/ConceptController.java#L406
  # Consequently chose to set includeDescendantCount as TRUE by default
  # to avoid misleading behaviour
  # This deviates from the default in snowstorm (includeDescendantCount = FALSE)

  stopifnot(is.character(conceptId))
  stopifnot(length(conceptId) == 1)
  stopifnot(!is.null(form))
  stopifnot(length(form) == 1)
  stopifnot(!is.na(form))
  stopifnot(form %in% c("inferred", "stated", "additional"))

  stopifnot(!is.null(includeDescendantCount),
            is.logical(includeDescendantCount) &
              !is.na(includeDescendantCount))

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "browser",
                     branch,
                     "concepts",
                     conceptId,
                     "parents")
  rest_url$query <- list(
    form = form,
    includeDescendantCount = includeDescendantCount
  )
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_browser_concept_descriptions <- function(
  term,
  active = NULL,
  module = NULL,
  language = NULL,
  type = NULL,
  semanticTag = NULL,
  semanticTags = NULL,
  preferredIn = NULL,
  acceptableIn = NULL,
  preferredOrAcceptableIn = NULL,
  conceptActive = NULL,
  conceptRefset = NULL,
  groupByConcept = FALSE,
  searchMode = "STANDARD",
  limit = snomedizer_options_get("limit"),
  offset = 0,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE
) {
  # GET /browser/{branch}/descriptions

  stopifnot(length(term) == 1)
  stopifnot(is.null(active) | (
    is.logical(active) &
      !is.na(active)
  ))
  module <- .concatenate_array_parameter(module)
  language <- .concatenate_array_parameter(language)
  type <- .concatenate_array_parameter(type)
  stopifnot(is.null(semanticTag) | length(semanticTag) == 1)
  semanticTags <- .concatenate_array_parameter(semanticTags)
  preferredIn <- .concatenate_array_parameter(preferredIn)
  acceptableIn <- .concatenate_array_parameter(acceptableIn)
  preferredOrAcceptableIn <- .concatenate_array_parameter(preferredOrAcceptableIn)
  stopifnot(is.null(conceptActive) | (
    is.logical(conceptActive) & !is.na(conceptActive)
  ))
  stopifnot(is.null(conceptRefset) | length(conceptRefset) == 1)
  stopifnot(is.null(groupByConcept) | (
    is.logical(groupByConcept) & !is.na(groupByConcept)
  ))
  stopifnot(searchMode %in% c("STANDARD", "REGEX"))
  stopifnot(is.null(offset) | length(offset) == 1)
  limit <- .validate_limit(limit)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "browser",
                     branch,
                     "descriptions")
  rest_url$query <- list(
    term = term,
    active = active,
    module = module,
    language = language,
    type = type,
    semanticTag = semanticTag,
    semanticTags = semanticTags,
    preferredIn = preferredIn,
    acceptableIn = acceptableIn,
    preferredOrAcceptableIn = preferredOrAcceptableIn,
    conceptActive = conceptActive,
    conceptRefset = conceptRefset,
    groupByConcept = groupByConcept,
    searchMode = "STANDARD",
    limit = limit,
    offset = offset
  )
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_descriptions_semantic_tags <- function(
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE) {
  # GET /{branch}/descriptions/semantictags

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "descriptions",
                     "semantictags")
  rest_result <- GET(rest_url)
  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_relationships <- function(
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  active = NULL,
  source = NULL,
  type = NULL,
  destination = NULL,
  characteristicType = NULL,
  limit = snomedizer_options_get("limit"),
  offset = 0,
  catch404 = TRUE,
  ...) {
  # GET /{branch}/relationships

  stopifnot(is.null(active) | length(active) == 1)
  stopifnot(is.null(source) | length(source) == 1)
  stopifnot(is.null(type) | length(type) == 1)
  stopifnot(is.null(destination) | length(destination) == 1)

  stopifnot(
    is.null(characteristicType) |
    characteristicType == "STATED_RELATIONSHIP" |
    characteristicType == "INFERRED_RELATIONSHIP" |
    characteristicType == "ADDITIONAL_RELATIONSHIP"
  )

  limit <- .validate_limit(limit)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "relationships")

  rest_url$query <- list(
    active = active,
    source = source,
    type = type,
    destination = destination,
    characteristicType = characteristicType,
    limit = limit,
    offset = offset
  )

  rest_url$query <- append(rest_url$query, list(...))
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}


#' @rdname api_operations
#' @export
api_relationship <- function(
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  relationshipId,
  catch404 = TRUE,
  ...) {
  # GET /{branch}/relationships/{relationshipId}

  stopifnot(length(relationshipId) == 1)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "relationships",
                     relationshipId)
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}


#' @rdname api_operations
#' @export
api_all_code_systems <- function(endpoint = snomedizer_options_get("endpoint"),
                                 forBranch = NULL,
                                 catch404 = TRUE) {
  # GET /codesystems

  if( !is.null(forBranch) ) {
    stopifnot(length(forBranch) == 1)
    stopifnot(is.character(forBranch))
  }

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "codesystems")
  rest_url$query <- list(
    forBranch = forBranch
  )
  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}


#' @rdname api_operations
#' @export
api_code_system <- function(endpoint = snomedizer_options_get("endpoint"),
                            shortName,
                            catch404 = TRUE) {
  # GET /codesystems/{shortName}

  stopifnot(length(shortName) == 1)
  stopifnot(is.character(shortName))

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "codesystems",
                     shortName)
  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}


#' @rdname api_operations
#' @export
api_code_system_all_versions <- function(endpoint = snomedizer_options_get("endpoint"),
                                         shortName,
                                         showFutureVersions = FALSE,
                                         showInternalReleases = FALSE,
                                         catch404 = TRUE) {
  # GET /codesystems/{shortName}/versions

  stopifnot(length(shortName) == 1)
  stopifnot(is.character(shortName))

  if( !is.null(showFutureVersions) ) {
    stopifnot(length(showFutureVersions)==1)
    stopifnot(is.logical(showFutureVersions))
  }

  if( !is.null(showInternalReleases) ) {
    stopifnot(length(showInternalReleases)==1)
    stopifnot(is.logical(showInternalReleases))
  }

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "codesystems",
                     shortName,
                     "versions")
  rest_url$query <- list(
    showFutureVersions = showFutureVersions
  )
  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}


#' @rdname api_operations
#' @export
api_browser_refset_members <- function(
  referenceSet = NULL,
  referenceSetModule = NULL,
  referencedComponentId = NULL,
  active = NULL,
  offset = NULL,
  searchAfter = NULL,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  limit = snomedizer_options_get("limit"),
  catch404 = TRUE
) {
  # GET /browser/{branch}/members

  # RF2 reference set descriptor data structure
  # https://confluence.ihtsdotools.org/display/DOCRELFMT/5.2.11+Reference+Set+Descriptor

  stopifnot(length(referenceSet) == 1 | is.null(referenceSet))
  stopifnot(length(referenceSetModule) == 1 | is.null(referenceSetModule))
  stopifnot(is.null(offset) | length(offset) == 1)
  referencedComponentId <- .concatenate_array_parameter(referencedComponentId)
  limit <- .validate_limit(limit)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     "browser",
                     branch,
                     "members")
  rest_url$query <- list(

    referenceSet = referenceSet,
    module = referenceSetModule,
    referencedComponentId = referencedComponentId,
    active = active,
    offset = offset,
    limit = limit,
    searchAfter = searchAfter
  )
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}


#' @rdname api_operations
#' @export
api_refset_members <- function(
  referenceSet = NULL,
  referenceSetModule = NULL,
  referencedComponentId = NULL,
  active = NULL,
  offset = NULL,
  searchAfter = NULL,
  targetComponent = NULL,
  mapTarget = NULL,
  owlExpression.conceptId = NULL,
  owlExpression.gci = NULL,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  limit = snomedizer_options_get("limit"),
  catch404 = TRUE
) {
  # GET /{branch}/members

  # RF2 reference set descriptor data structure
  # https://confluence.ihtsdotools.org/display/DOCRELFMT/5.2.11+Reference+Set+Descriptor

  stopifnot(length(referenceSet) == 1 | is.null(referenceSet))
  stopifnot(length(referenceSetModule) == 1 | is.null(referenceSetModule))
  stopifnot(length(targetComponent) == 1 | is.null(targetComponent))
  stopifnot(length(mapTarget) == 1 | is.null(mapTarget))
  stopifnot(length(owlExpression.conceptId) == 1 | is.null(owlExpression.conceptId))
  stopifnot(length(owlExpression.gci) == 1 | is.null(owlExpression.gci))
  stopifnot(is.null(offset) | length(offset) == 1)
  referencedComponentId <- .concatenate_array_parameter(referencedComponentId)
  limit <- .validate_limit(limit)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path[rest_url$path != ""],
                     branch,
                     "members")
  rest_url$query <- list(
    referenceSet = referenceSet,
    module = referenceSetModule,
    referencedComponentId = referencedComponentId,
    active = active,
    offset = offset,
    searchAfter = searchAfter,
    targetComponent = targetComponent,
    mapTarget = mapTarget,
    owlExpression.conceptId = owlExpression.conceptId,
    owlExpression.gci = owlExpression.gci,
    limit = limit
  )
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch_http_error(rest_result)
  }

  rest_result
}


