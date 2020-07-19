
#' SNOMED CT Terminology Server REST API operations
#'
#' @description Low-level wrapper functions based on \link{httr} for interfacing
#' with the operations built in the \href{https://github.com/IHTSDO/snowstorm}{Snowstorm API}
#' @param accept_language a string specifying acceptable result languages
#' @param activeFilter optional boolean: \itemize{
#'     \item \code{TRUE} returns only active terminology
#'     \item \code{FALSE} returns only inactive terminology
#'     \item \code{NULL} (the default) returns both active and inactive terminology
#' }
#' @param catch404 whether to display a warning if the API operation returns a
#' '404 Not Found' status. Default is \code{TRUE}.
#' @param concept character string of a SNOMED-CT concept id (for example:
#' \code{"233604007"})
#' @param conceptId character string of a SNOMED-CT concept id (for example:
#' \code{"233604007"})
#' @param conceptIds a character vector of SNOMED-CT concept ids (for example:
#' \code{c("233604007", "68566005")})
#' @param descendantCountForm a character string indicating whether to report
#' the count of descendant concepts based on stated or inferred relationships.
#' Must be one of \code{"inferred"}, \code{"stated"}, or \code{"additional"}.
#' Default is \code{NULL} for no descendant count reported.
#' @param endpoint the URL of a SNOMED CT Terminology Server REST API endpoint.
#'  See \code{\link{snomedizer_options}}.
#' @param form a character string indicating which ancestors/descendants to
#' extract based on stated or inferred relationships. Must be one of
#' \code{"inferred"} (default), \code{"stated"}, or \code{"additional"}.
#' @param branch a string for the name of the API endpoint branch to use (most
#' commonly \code{"MAIN"}). See \code{\link{snomedizer_options}}.
#' @param limit integer for the maximum number of results to return.
#' See \code{\link{snomedizer_options}}.
#' @param offset an integer indicating the number of results to skip
#' @param term character vector of terms to search
#'
#' @param ... other REST API parameters
#' @importFrom httr parse_url build_url GET
#' @return An \code{httr} \code{\link[httr]{response}()} object.
#' @name api_operations
#' @examples
#' # look up the pneumonia concept
#' api_find_concept(conceptId = "233604007")
#' api_find_concepts(term = "pneumonia")
#' api_find_concepts(conceptIds = c("233604007", "68566005"))
#'
#' # get the content of the server request
#' httr::content(api_find_concepts(term = "pneumonia"))
NULL

#' @rdname api_operations
#' @export
api_find_concept <- function(conceptId,
                             endpoint = snomedizer_options_get("endpoint"),
                             branch = snomedizer_options_get("branch"),
                             accept_language = snomedizer_options_get("language"),
                             catch404 = TRUE) {


  stopifnot(is.character(conceptId))

  if ( length(conceptId) > 1) {
    conceptId <- conceptId[[1]]
    warning("Several `conceptId` provided. Only the first value will be used.")
  }

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path,
                     branch,
                     "concepts",
                     conceptId)
  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404) {
    .catch404(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_find_concepts <- function(
  term = NULL,
  conceptIds = NULL,
  activeFilter = NULL,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  limit = snomedizer_options_get("limit"),
  accept_language = snomedizer_options_get("language"),
  offset = 0,
  catch404 = TRUE,
  ...
) {

  if(length(conceptIds>1)){
    # `AsIs` used to prevent URL encoding of the ampersand
    # by httr:::compose_query (curl::curl_escape)
    conceptIds <- I(
      paste(conceptIds,
            collapse = paste0("&conceptIds="))
    )
  }

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path,
                     branch,
                     "concepts")
  rest_url$query <- list(
    term = term,
    conceptIds = conceptIds,
    limit = limit,
    offset = offset,
    activeFilter = activeFilter
  )
  rest_url$query <- append(rest_url$query, list(...))
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch404(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_all_branches <- function(endpoint = snomedizer_options_get("endpoint"),
                             catch404 = TRUE) {
  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path,
                     "branches")
  rest_result <- GET(rest_url)

  if(catch404){
    .catch404(rest_result)
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
  rest_url$path <- c(rest_url$path,
                     "branches",
                     branch)
  rest_url$query <- list(...)
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch404(rest_result)
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
  rest_url$path <- c(rest_url$path,
                     "branches",
                     branch,
                     "children")
  rest_url$query <- list(...)
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch404(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_descriptions <- function(
  concept,
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  offset = 0,
  limit = snomedizer_options_get("limit"),
  catch404 = TRUE,
  ...) {

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path,
                     branch,
                     "descriptions")
  rest_url$query <- list(
    concept = concept,
    offset = offset,
    limit = limit
  )
  rest_url$query <- append(rest_url$query, list(...))
  .check_rest_query_length1(rest_url)

  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch404(rest_result)
  }

  rest_result
}


#' @rdname api_operations
#' @export
api_version <- function(
  endpoint = snomedizer_options_get("endpoint"),
  catch404 = TRUE) {

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path,
                     "version")

  rest_result <- GET(rest_url)

  if(catch404){
    .catch404(rest_result)
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
    accept_language = snomedizer_options_get("language"),
    catch404 = TRUE
  ) {

  descendantCountForm <- descendantCountForm[1]
  if(!is.null(descendantCountForm)) {
    stopifnot(descendantCountForm %in% c("inferred", "stated", "additional"))
  }

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path,
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
    .catch404(rest_result)
  }

  rest_result
}

#' @rdname api_operations
#' @export
api_browser_concept_ancestors <- function(
  conceptId,
  form = c("inferred", "stated", "additional"),
  endpoint = snomedizer_options_get("endpoint"),
  branch = snomedizer_options_get("branch"),
  accept_language = snomedizer_options_get("language"),
  catch404 = TRUE
) {

  form <- form[1]
  if(!is.null(form)) {
    stopifnot(form %in% c("inferred", "stated", "additional"))
  }

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path,
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
    .catch404(rest_result)
  }

  rest_result
}
