
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
#' @family utilities
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
    limit = getOption("snomedizer.limit")
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
#' @family utilities
#' @export
snomed_public_endpoint_suggest <- function() {
  snomed_public_endpoints <- gsub("/*$", "", list(
    "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct/",
    "https://browser.ihtsdotools.org/snowstorm/snomed-ct/",
    "https://snowstorm.test-nictiz.nl/",
    "https://snowstorm.msal.gov.ar/"
  ))

  for(i in seq_along(snomed_public_endpoints)) {
    endpoint_answers <- try(httr::http_error(snomed_public_endpoints[[i]]))
    if (!methods::is(endpoint_answers, "try-error") && !endpoint_answers) {
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


#' Test a SNOMED-CT endpoint
#'
#' @param endpoint a character URL to a SNOMED-CT endpoint
#' @param branch a character string of a branch name
#'
#' @return a boolean indicating whether the endpoint passed the test
#' @export
#'
#' @examples
#' snomed_endpoint_test(
#'   endpoint = "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct",
#'   branch = "MAIN"
#' )
snomed_endpoint_test <- function(endpoint, branch) {

  output <- api_concept(conceptId = "233604007",
                             endpoint = endpoint,
                             branch = branch)

  # Determine whether the status code starts with a 2
  return(
    output$status_code - (output$status_code %% 100) == 200
  )
}


#' Flatten results from a server request
#'
#' @description A function to \code{\link[jsonlite]{flatten}}
#' nested server results from an \code{\link{api_operations}}
#'  into a single non-nested data frame
#' @param x an `httr` \code{\link[httr]{response}()} object
#' @return a data frame
#' @export
#' @family utilities
#' @examples
#' result_flatten(
#'    api_concepts(term = "pneumonia",
#'    activeFilter = TRUE,
#'    limit = 10))
result_flatten <- function(x) {
  x <- httr::content(x, as = 'text')
  x <- jsonlite::fromJSON(x, flatten = TRUE)
  empty_index <- sapply(x, length) == 0
  if(any(empty_index)) {
    x[empty_index] <- NA
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  colnames(x) <- gsub("^items.", "", colnames(x))

  x
}


#' Check that results are complete
#'
#' @description Check whether the server request returned all the results,
#' i.e. whether the `limit` < results `total`.
#' @param x an `httr` \code{\link[httr]{response}()} object produce by an
#' \code{\link{api_operations}} function.
#' @param silent whether to display warnings (default is `FALSE`)
#'
#' @return a boolean indicating whether the set of results obtained is
#' the complete set of results on the server.
#' @export
#' @family utilities
#' @examples
#' result_completeness(api_concepts(term = "pneumonia", limit = 10))
result_completeness <- function(x, silent = FALSE) {
  stopifnot(methods::is(x, "response"))
  complete <- httr::content(x)$total <= httr::content(x)$limit

  if(!complete & !silent) {
    warning(paste0(
      "\nThis server request returned just ", httr::content(x)$limit,
      " of a total ", httr::content(x)$total, " results.",
      "\nPlease increase the server `limit` to fetch all results."
    ), call. = FALSE)
  }

  complete
}


#' Catch 404 errors in a server request
#'
#' @param x a \code{\link{httr}{response}} object from a server request
#' @keywords internal
.catch404 <- function(x) {
  if(x$status_code == 404) {
    warning(simpleWarning("404 Not Found"))
  }
}


#' Check that all REST query parameters have length 1
#'
#' @param rest_url a list of class \code{url} generated by \code{\link[httr]{parse_url}()}
#' and containing a \code{query} list
#' @keywords internal
.check_rest_query_length1 <- function(rest_url) {
  if(any(sapply(rest_url$query, length) > 1)){
    stop(paste0("The following arguments must have length <= 1: `",
                paste(names(rest_url$query)[length(rest_url$query) > 1], collapse = "`, `"), "`"))
  }
}


#' Concatenated REST query array parameters
#'
#' @param param a character vector of values to concatenate
#' @return a character string
#' @keywords internal
.concatenate_array_parameter <- function(param) {
  if(length(param>1)){
    # `AsIs` used to prevent URL encoding of the ampersand
    # by httr:::compose_query (curl::curl_escape)
    param <- I(
      paste(param,
            collapse = paste0("&", substitute(param), "="))
    )
  }

  param
}

