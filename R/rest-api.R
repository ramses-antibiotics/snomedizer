
#' Default \code{snomedizer} options
#'
#' @description Functions to get and set \code{snomedizer} default endpoint and other options
#' @param option.name the name of an option to return. If NULL (the default),
#' \code{snomedizer_options_get()} returns a list of all options.
#' @param endpoint URL of a SNOMED CT Terminology Server REST API endpoint.
#' When the package is loaded, this option is set to the current environment variable
#' \code{SNOMEDIZER_ENDPOINT} if it exists, or to
#' \code{\link{snomed_public_endpoint_suggest}()} otherwise.
#' @param branch string for the branch to use on the SNOMED CT Terminology Server REST API
#' endpoint. Set to `"MAIN"` by default when the package is loaded.
#' @param limit integer for the maximum number of results to return. Set to 50 by default
#' when the package is loaded.
#' @details  The SNOMED REST API requires several parameters which are
#' set to default values when this package is launched:
#' \itemize{
#'    \item{endpoint} this is the address of the SNOWSTORM terminology server
#'    to be used. When \code{snomedizer} is loaded, it is set to the current
#'    environment variable \code{SNOMEDIZER_ENDPOINT}. If no such
#'     the SNOMED-CT version maintained by the official
#'         SNOWSTORM server.
#'    \item{branch} The default is "MAIN/SNOMEDCT-GB", the most
#'         up-to-date edition of SNOMED-CT UK Core Edition.
#'    \item{limit} an integer stating the the maximum number of results fetched. This is set to 50 The default
#'         is 50.
#' }
#'
#' @return The factory setting of the target API parameter.
#' @seealso To learn how to set environment variables in `.Rprofile` or `.Renviron`, see
#' \link[base]{Startup}
#' @examples
#' snomedizer_options_get()
#' @name snomedizer_options
NULL

#' @rdname snomedizer_options
#' @export
snomedizer_options_get <- function(option.name = NULL){
  default_options <- list(
    endpoint = getOption("snomedizer.endpoint"),
    branch = getOption("snomedizer.branch"),
    limit = getOption("snomedizer.limit"),
    language = getOption("snomedizer.language")
  )
  if ( any(is.null(default_options)) ) {
    print(default_options)
    warning("Invalid snomedizer default options. See `snomedizer_options_set()`")
  }

  if (length(option.name) == 1) {
    return(
      default_options[[
        grep(option.name, names(default_options))
        ]]
    )
  } else {
    return(default_options)
  }
}

#' @rdname snomedizer_options
#' @export
snomedizer_options_set <- function(endpoint = NULL,
                                   branch = NULL, limit = NULL) {

  if (all(sapply(list(endpoint, branch, limit), is.null))) {
    stop("Please provide at least one input.")
  }

  if (!is.null(branch)) {
    stopifnot(length(branch) == 1)
    stopifnot(!is.na(branch))
    stopifnot(is.character(branch))
    stopifnot(branch != "")
    options(snomedizer.branch = utils::URLencode(branch))
  }

  if (!is.null(limit)) {
    stopifnot(length(limit) == 1)
    stopifnot(is.integer(limit))
    stopifnot(!is.na(limit))
    options(snomedizer.limit = limit)
  }

  if (!is.null(endpoint)) {
    endpoint <- utils::URLencode(gsub("/*$", "", endpoint))
    stopifnot(length(endpoint) == 1)
    stopifnot(is.character(endpoint))
    if(httr::http_error(endpoint)) {
      stop("The provided `endpoint` is not responding.")
    }
    if(!snomed_endpoint_test(endpoint = endpoint,
                             branch = snomedizer_options_get()$branch)) {
      stop("`endpoint` and `branch` are not returning valid answers.")
    }
    options(snomedizer.endpoint = endpoint)
  } else {
    if(!snomed_endpoint_test(endpoint = snomedizer_options_get()$endpoint,
                             branch = snomedizer_options_get()$branch)) {
      stop("`endpoint` is not returning valid answers.")
    }

  }

  invisible()
}


#' Find a public SNOMED-CT endpoint
#'
#' @return a string object containing the URL to a responsive SNOMED CT Terminology Server REST API endpoint.
#' @export
snomed_public_endpoint_suggest <- function() {
  snomed_public_endpoints <- gsub("/*$", "", list(
    "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct/",
    "https://browser.ihtsdotools.org/snowstorm/snomed-ct/",
    "https://snowstorm.test-nictiz.nl/",
    "https://snowstorm.msal.gov.ar/"
  ))

  for(i in seq_along(snomed_public_endpoints)) {
    if (!httr::http_error(snomed_public_endpoints[[i]])) {
      endpoint <- snomed_public_endpoints[[i]]
      break
    }
  }

  if(!exists("endpoint")) {
    stop("No working SNOMED endpoint found. Try again later.")
  } else {
    return(endpoint)
  }
}


snomed_endpoint_test <- function(endpoint, branch) {

  output <- api_find_concept(conceptId = "233604007",
                             endpoint = endpoint,
                             branch = branch)

  # Determine whether the status code starts with a 2
  return(
    output$status_code - (output$status_code %% 100) == 200
  )
}


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

  .catch404(rest_result, catch = catch404)

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

  term <- .rest_arguments_listify(term)
  conceptIds <- .rest_arguments_listify(conceptIds)

  rest_url <- httr::parse_url(endpoint)
  rest_url$path <- c(rest_url$path,
                     branch,
                     "concepts")
  rest_url$query <- c(
    term, conceptIds
  )
  rest_url <- httr::build_url(rest_url)
  rest_result <- GET(rest_url)

  .catch404(rest_result, catch = catch404)

  rest_result
}


.rest_arguments_listify <- function(x){
  if(!is.null(x)){
    names(x) <- rep(deparse(substitute(x)), length(x))
    x <- as.list(x)
  }
  x
}


.catch404 <- function(x, catch) {
  if(catch == TRUE & x$status_code == 404) {
    warning(simpleWarning("404 Not Found"))
  }
}


#' snowstorm_get <- function(query) {
#'   x <- jsonlite::parse_json(curl::curl(query))
#'
#'   if (exists("limit", x) & x$limit < x$total) {
#'     warning(paste(x$total, "concepts were found Increase limit to extract them all."))
#'   }
#'
#'   x
#' }
#'
#' snowstorm_base_url <- function() {
#'   paste0(
#'     "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct/",
#'     utils::URLencode(getOption("snowstorm.branch"), reserved = T),
#'     "/"
#'   )
#' }
#'
#' snowstorm_endpoint_test <- function()
#'
#' snowstorm_browser_url <- function() {
#'   paste0(
#'     "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct/browser/",
#'     utils::URLencode(getOption("snowstorm.branch"), reserved = T),
#'     "/"
#'   )
#' }
#'
#' choose_api_endpoint <- function() {
#'
#'   ""
#' }
#'
#' snowstorm_branch_info <- function() {
#'   query <- curl::curl(paste0(
#'     "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct/branches/",
#'     utils::URLencode(getOption("snowstorm.branch"), reserved = T),
#'     "?includeInheritedMetadata=true"
#'   ))
#'
#'   jsonlite::parse_json(query)
#' }
#'
#' snowstorm_search_term <- function(term,
#'                                   limit = getOption("snowstorm.limit")) {
#'   query <- (paste0(
#'     snowstorm_base_url(),
#'     "concepts?activeFilter=true&term=",
#'     utils::URLencode(tolower(term), reserved = T),
#'     "&limit=",
#'     limit
#'   ))
#'
#'   snowstorm_get(query)
#' }
#'
#'
#' snowstorm_fetch_concepts <- function(concept_ids) {
#'   concept_ids <- as.list(concept_ids)
#'
#'   query <- (paste0(
#'     snowstorm_base_url(),
#'     "concepts?",
#'     paste(
#'       paste0("conceptIds=", concept_ids, collapse = "&")
#'     )
#'   ))
#'
#'   snowstorm_get(query)
#' }
#'
#'
#' snowstorm_fetch_children <- function(concept_ids, direct = F,
#'                                      limit = getOption("snowstorm.limit")){
#'
#'   if(length(concept_ids)==1){
#'     snowstorm_fetch_children_single(concept_ids, direct = direct, limit = limit)
#'   } else {
#'     x <- purrr::map_df(concept_ids, function(X){
#'       snowstorm_fetch_children_single(X, direct = direct, limit = limit) %>%
#'         dplyr::mutate(parentId = as.character(X))
#'     }) %>%
#'       dplyr::bind_rows()
#'   }
#'
#' }
#'
#'
#' snowstorm_fetch_children_single <- function(concept_id, direct, limit) {
#'   if (length(concept_id) > 1) {
#'     stop()
#'   }
#'
#'   if (direct) {
#'     query <- (paste0(
#'       snowstorm_browser_url(),
#'       "concepts/", concept_id,
#'       "/children?form=inferred&includeDescendantCount=true&limit=",
#'       limit
#'     ))
#'   } else {
#'     query <- (paste0(
#'       snowstorm_base_url(),
#'       "concepts/", concept_id,
#'       "/descendants?stated=false&limit=",
#'       limit
#'     ))
#'   }
#'
#'   snowstorm_get(query)$items %>%
#'     purrr::map_df(., purrr::flatten) %>%
#'     dplyr::bind_rows()
#' }
#'
#'
#' snowstorm_fetch_parent <- function(concept_id, direct = F,
#'                                    limit = getOption("snowstorm.limit")) {
#'   if (length(concept_id) > 1) {
#'     stop("concept_id must be a character vector of length == 1")
#'   }
#'
#'   if (direct) {
#'     query <- (paste0(
#'       snowstorm_browser_url(),
#'       "concepts/", concept_id,
#'       "/children?form=inferred&includeDescendantCount=true&limit=",
#'       limit
#'     ))
#'   } else {
#'     query <- (paste0(
#'       snowstorm_base_url(),
#'       "concepts/", concept_id,
#'       "/descendants?stated=false&limit=",
#'       limit
#'     ))
#'   }
#'
#'   snowstorm_get(query)
#' }
#'
#'
#' snowstorm_fetch_relationships <- function(concept_id, limit = getOption("snowstorm.limit")) {
#'   if (length(concept_id) > 1) {
#'     stop("concept_id must be a character vector of length == 1")
#'   }
#'
#'   query <- paste0(
#'     snowstorm_base_url(),
#'     "relationships?active=true&module=900000000000207008&source=", concept_id,
#'     "&limit=", limit
#'   )
#'
#'   snowstorm_get(query)
#' }
#'
#'
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
#' snowstorm_fetch_descriptions <- function(concept_ids) {
#'   query <- paste0(
#'     snowstorm_base_url(),
#'     "descriptions?concept=",
#'     concept_ids
#'   )
#'
#'   query <- as.list(query)
#'
#'   inf_data <- purrr::map(query, function(X) snowstorm_get(X)$items)
#'
#'   inf_data <- purrr::map_df(inf_data, function(X) {
#'     purrr::map_df(X, function(Y) {
#'       purrr::flatten_df(Y)[, c(
#'         "conceptId", "descriptionId", "term",
#'         "type", "caseSignificance", "active"
#'       )]
#'     })
#'   })
#'
#'   inf_data
#' }
#'

