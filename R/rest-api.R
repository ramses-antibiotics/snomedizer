
#' SNOMED CT Terminology Server REST API operations
#'
#' @description Low-level wrapper functions based on \link{httr} for interfacing
#' with the operations built in the \href{https://github.com/IHTSDO/snowstorm}{Snowstorm API}
#' @param accept_language a string specifying acceptable result languages
#' @param activeFilter optional boolean: \itemize{
#'     \item `TRUE` returns only active terminology
#'     \item `FALSE` returns only inactive terminology
#'     \item `NULL` (the default) returns both active and inactive terminology
#' }
#' @param catch404 whether to display a warning if the API operation returns a
#' '404 Not Found' status. Default is `TRUE`.
#' @param conceptId a string
#' @param conceptIds a character vector of SNOMED-CT concept ids (for example:
#' \code{c("233604007", "68566005")})
#' @param endpoint the URL of a SNOMED CT Terminology Server REST API endpoint.
#'  See \code{\link{snomedizer_options}}.
#' @param branch a string for the name of the API endpoint branch to use (most
#' commonly \code{"MAIN"}). See \code{\link{snomedizer_options}}.
#' @param limit integer for the maximum number of results to return.
#' See \code{\link{snomedizer_options}}.
#' @param offset an integer indicating the number of results to skip
#' @param term character vector of terms to search
#'
#' @param ... other REST API parameters
#' @importFrom httr parse_url build_url GET
#' @return An `httr` \code{\link[httr]{response}()} object.
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
  if(any(sapply(rest_url$query, length) > 1)){
    stop(paste0("The following arguments must have length <= 1: `",
               paste(names(rest_url$query)[length(rest_url$query) > 1], collapse = "`, `"), "`"))
  }
  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  if(catch404){
    .catch404(rest_result)
  }

  rest_result
}


# api_browser_find_concept_children <- function(){
#
# }





