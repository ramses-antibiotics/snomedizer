
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
#' @param conceptId character string of a SNOMED-CT concept id (for example:
#' \code{"233604007"})
#' @param conceptIds a character vector of SNOMED-CT concept ids (for example:
#' \code{c("233604007", "68566005")})
#' @param conceptActive optional boolean: \itemize{
#'     \item \code{TRUE} returns only active concepts
#'     \item \code{FALSE} returns only inactive concepts
#'     \item \code{NULL} (the default) returns both active and inactive concepts
#' }
#' @param conceptRefset character vector of reference sets concept ids
#' to include (example: \code{"900000000000497000"} for CTV3 terminology).
#' See \code{api_concept_descendants("900000000000455006")}
#' for valid reference set concepts.
#' @param descendantCountForm a character string indicating whether to report
#' the count of descendant concepts based on stated or inferred relationships.
#' Must be one of \code{"inferred"}, \code{"stated"}, or \code{"additional"}.
#' Default is \code{NULL} for no descendant count reported.
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
#' @param groupByConcept a boolean indicating whether to group descriptions
#' by concept. Default is \code{FALSE}.
#' @param includeDescendantCount a boolean indicating whether a number of
#' children/descendants counter should be included in the result
#' @param language vector of two-character language codes to include
#' (example: \code{c("en", "de")}).
#' @param limit a positive integer for the maximum number of results to return.
#' See \code{\link{snomedizer_options}}. The maximum limit on public endpoints
#' is 10,000.
#' @param module character vector of SNOMED-CT modules to include (example:
#' \code{"900000000000207008"})
#' @param offset an integer indicating the number of results to skip
#' @param preferredIn character vector of description language reference sets
#' (example: \code{"900000000000509007"}).
#' The description must be preferred in at least one of these to match.
#' @param preferredOrAcceptableIn character vector of description language reference sets
#' (example: \code{"900000000000509007"}).
#' The description must be preferred OR acceptable in at least one of these to match.
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
#' @param stated a boolean indicating whether to limit search to descendants
#' whose relationship is stated rather than inferred. Default is \code{FALSE}.
#' @param term character vector of terms to search
#' @param type character vector of description types to include. See
#' \code{api_concept_descendants("900000000000446008")} for valid
#' description type inputs.
#' @param ... other REST API parameters
#' @importFrom httr parse_url build_url GET
#' @return An \code{httr} \code{\link[httr]{response}()} object.
#' @name api_operations
#' @family api_operations
#' @section Disclaimer:
#' In order to use SNOMED-CT, a licence is required which depends both on the country you are
#' based in, and the purpose of your work. See details on \link{snomedizer}.
#' @examples
#' # look up the pneumonia concept
#' api_concept(conceptId = "233604007")
#' api_concepts(term = "pneumonia")
#' api_concepts(conceptIds = c("233604007", "68566005"))
#'
#' # get the content of the server request
#' httr::content(api_concepts(term = "pneumonia"), limit = 1)
NULL

#' @rdname api_operations
#' @export
api_concept <- function(conceptId,
                        endpoint = snomedizer_options_get("endpoint"),
                        branch = snomedizer_options_get("branch"),
                        catch404 = TRUE) {


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
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  limit = snomedizer_options_get("limit"),
  offset = 0,
  catch404 = TRUE,
  ...
) {

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
    activeFilter = activeFilter
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
  includeDescendantCount = FALSE,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  catch404 = TRUE
) {

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

  # TODO: Unlike api_browser_concept_children, this only provides leaf flag
  # if includeDescendantCount = TRUE
  # https://github.com/IHTSDO/snowstorm/blob/07e3f7ea08f8091b20f856a4775f867608961329/src/main/java/org/snomed/snowstorm/rest/ConceptController.java#L406
  # Consequently chose to set includeDescendantCount as TRUE by default
  # to avoid misleading behaviour - since this goes against the Rd documented
  # default (FALSE) will need to review this in the future

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




