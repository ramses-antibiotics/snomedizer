
#' Set SNOMED CT endpoint and other \code{snomedizer} options
#'
#' @description Functions to get and set \code{snomedizer} default endpoint and other options
#' @param option.name name of a single option to return: \code{"endpoint"},
#' \code{"branch"}, or \code{"limit"}. If \code{NULL} (the default),
#' \code{snomedizer_options_get()} returns a list of all options.
#' @param endpoint address of a SNOMED CT Terminology Server REST API endpoint
#' @param branch string for the branch name to use on endpoint, for instance
#' \code{"MAIN"} for the root branch (usually the latest release of
#' SNOMED CT's International Edition), or \code{"MAIN/2017-07-31"} for a past release.
#' To obtain a list of all branches available on the current endpoint,
#' see \code{\link{api_branch}()}
#' @param limit integer for the maximum number of results to return.
#' @section Default settings and environment variables:
#'
#' When loaded, the snomedizer package will look up for settings provided
#' to the following environment variables:
#' \describe{
#'    \item{\code{SNOMEDIZER_ENDPOINT}}{for the \code{endpoint}. If this variable
#'    is not specified, snomedizer uses
#'    \code{\link{snomed_public_endpoint_suggest}()} to pick a public endpoint.}
#'    \item{\code{SNOMEDIZER_BRANCH}}{for the \code{branch}. If this variable is
#'    not specified, snomedizer chooses branch \code{"MAIN"} by default.}
#'    \item{\code{SNOMEDIZER_LIMIT}}{for the \code{limit}. If this variable is
#'    not specified, snomedizer sets the limit to 50 by default.
#'    The maximum is 10,000.}
#' }
#'
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

  if ( !is.null(option.name) &&
       option.name != "endpoint" &&
       option.name != "branch" &&
       option.name != "limit" ) {
    stop("`option.name` must be of length 1 and equal to \"endpoint\", \"branch\" or \"limit\"")
  }

  default_options <- list(
    endpoint = getOption("snomedizer.endpoint"),
    branch = getOption("snomedizer.branch"),
    limit = getOption("snomedizer.limit")
  )

  if ( any(is.null(default_options)) ) {
    print(default_options)
    warning("Invalid snomedizer default options. See `?snomedizer_options_set`")
  }

  if ( !is.null(option.name) ) {
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
                                   branch = NULL,
                                   limit = NULL) {

  if (all(sapply(list(endpoint, branch, limit), is.null))) {
    stop("Please provide at least one input.")
  }

  if (!is.null(limit)) {
    limit <- .validate_limit(limit)
    options(snomedizer.limit = limit)
  }

  if (is.null(branch)) {
    branch <- ifelse(
      is.null(snomedizer_options_get("branch")),
      "MAIN",
      snomedizer_options_get("branch"))
  } else {
    branch <- .validate_branch(branch)
  }

  if (!is.null(endpoint)) {

    endpoint <- utils::URLencode(gsub("/*$", "", endpoint))
    stopifnot(length(endpoint) == 1)
    stopifnot(is.character(endpoint))

    if ( httr::http_error(httr::GET(endpoint)) ) {
      stop("The provided `endpoint` is not responding.")
    }

    if (
      !snomed_endpoint_test(
        endpoint = endpoint,
        branch = branch
      )
    ) {
      stop("`endpoint` and `branch` are not returning valid answers.")
    } else {
      options(snomedizer.endpoint = endpoint)
      options(snomedizer.branch = branch)
      test <- snomedizer_version_compatibility()
    }

  } else {
    if(!snomed_endpoint_test(endpoint = snomedizer_options_get()$endpoint,
                             branch = branch)) {
      stop("`endpoint` is not returning valid answers.")
    } else {
      options(snomedizer.branch = branch)
      test <- snomedizer_version_compatibility()
    }
  }

  invisible()
}


#' Find a public SNOMED CT endpoint
#'
#' @return a string object containing the URL to a responsive SNOMED CT Terminology Server REST API endpoint.
#' @family utilities
#' @export
snomed_public_endpoint_suggest <- function() {

  snomed_public_endpoints <- gsub(
    "/*$", "",
    snomed_public_endpoint_list()
  )

  for(i in seq_along(snomed_public_endpoints)) {
    endpoint_answers <- try(httr::http_error(httr::GET(snomed_public_endpoints[[i]])))
    if (!methods::is(endpoint_answers, "try-error") && !endpoint_answers) {
      endpoint <- snomed_public_endpoints[[i]]
      break
    }
  }

  if(!exists("endpoint")) {
    warning("No working SNOMED endpoint found. Try again later.")
    return(NULL)
  } else {
    test <- snomedizer_version_compatibility(endpoint = endpoint)
    return(endpoint)
  }
}


#' List of public SNOMED CT endpoints
#'
#' @description List a range of know public SNOMED CT Terminology Server
#' REST API endpoint. To select a currently active endpoint, see
#' \code{\link{snomed_public_endpoint_suggest}()}
#' @return a vector of URLs
#' @family utilities
#' @export
snomed_public_endpoint_list <- function() {

  as.list(
    readLines(
      system.file(
        package = "snomedizer",
        "public_snomed_endpoints"
      )
    )
  )

}

#' Test a SNOMED CT endpoint
#'
#' @param endpoint URL of a SNOMED CT Terminology Server REST API endpoint
#' @param branch a character string of a branch name
#'
#' @return a boolean indicating whether the endpoint passed the test
#' @export
#' @family utilities#'
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


#' Verify the endpoint compatibility with snomedizer
#'
#' @description This function compares the SNOMED CT terminology server endpoint version
#' with `snomedizer`'s supported version. The SNOMED CT terminology server API
#' is continuously developed and may introduce breaking changes in REST operations
#' and parameters.
#' @param endpoint URL of a SNOMED CT Terminology Server REST API endpoint
#' @param silent whether to hide warnings. Default is `FALSE`
#' @return a logical value indicating whether the endpoint version is supported
#' @export
#' @family utilities
snomedizer_version_compatibility <- function(
  endpoint = snomedizer_options_get("endpoint"),
  silent = FALSE
) {

  endpoint_version <- httr::content(api_version(endpoint = endpoint))$version
  endpoint_version_num <- as.numeric(unlist(strsplit(endpoint_version, "[.]")))

  version_main <- endpoint_version_num[1]
  version_minor <- (endpoint_version_num[2] + endpoint_version_num[3] * 0.001)

  if (
    (version_main < 7) |
    (version_main == 7 & version_minor < 6)
  ) {
    warning(
      paste(
        paste0("The selected endpoint version is ", endpoint_version, "."),
        "This version of snomedizer is designed for endpoint versions 7.6.0 or greater.",
        "Some function may not work as intended.",
        sep = "\n"
      )
    )
    return(FALSE)
  } else {
    return(TRUE)
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

  ct_version <- concept_descriptions(
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


#' Flatten results from a server request
#'
#' @description A function to \code{\link[jsonlite]{flatten}}
#' nested server results from an \code{\link{api_operations}}
#'  into a single non-nested data frame
#' @param x an `httr` \code{\link[httr]{response}()} object
#' @param encoding HTTP charset parameter to use (default is \code{"UTF-8"})
#' @return a data frame
#' @export
#' @family utilities
#' @examples
#' flattened_results <- result_flatten(
#'    api_concepts(term = "pneumonia",
#'    activeFilter = TRUE,
#'    limit = 10))
#' str(flattened_results)
result_flatten <- function(x, encoding = "UTF-8") {
  x <- httr::content(x, as = "text", encoding = encoding)
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
#' i.e. whether the total number of results is smaller or equal to the
#' request \code{limit} parameter.
#' @param x an `httr` \code{\link[httr]{response}()} object produce by an
#' \code{\link{api_operations}} function.
#' @param silent whether to display warnings (default is `FALSE`)
#'
#' @return a boolean indicating whether the set of results obtained is
#' the complete set of results on the server. If the REST request failed
#' (eg: 404 error), the function returns \code{NULL}
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


#' Catch HTTP errors in a server request
#'
#' @param x a \code{\link{httr}{response}} object from a server request
#' @keywords internal
#' @noRd
.catch_http_error <- function(x) {
  if(httr::http_error(x)) {
    warning(paste0(
      "Status ", x$status_code, " ", httr::content(x, encoding = "UTF-8")$error,
      "\n", httr::content(x, encoding = "UTF-8")$message
    ), call. = FALSE)
  }
}


#' Check that all REST query parameters have length 1
#'
#' @param rest_url a list of class \code{url} generated by \code{\link[httr]{parse_url}()}
#' and containing a \code{query} list
#' @keywords internal
#' @noRd
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
#' @noRd
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


.validate_limit <- function(limit) {
  if(length(limit) != 1) {
    stop("`limit` must have length == 1")
  }
  if(is.null(limit) || is.na(limit)) {
    stop("`limit` must not be NULL or missing")
  }
  if(!is.numeric(limit) || limit < 0 ||
     # check is whole number
     abs(limit - round(limit)) >= .Machine$double.eps^0.5) {
    stop("`limit` must be a strictly positive integer")
  }
  if(limit > 10000){
    # This is controlled by Java class
    # org.snomed.snowstorm.rest.ControllerHelper
    # https://github.com/IHTSDO/snowstorm/blob/master/src/main/java/org/snomed/snowstorm/rest/ControllerHelper.java#L212
    warning("Please note the maximum limit on public endpoints is 10,000.")
  }

  as.integer(limit)
}

.validate_branch <- function(branch) {

  # See snowstorm documentation
  # https://github.com/IHTSDO/snowstorm/blob/master/docs/branching-and-merging.md

  # Branch names are controlled by Java class
  # io.kaicode.elasticvc.api.BranchService
  # https://github.com/kaicode/elasticvc/blob/master/src/main/java/io/kaicode/elasticvc/api/BranchService.java#L75
  # They are hierarchical.
  # The root branch is "MAIN"
  # Derived SNOMED CT Editions branch off, eg "MAIN/SNOMEDCT-UK"
  # Past releases branch off too, eg "MAIN/2017-07-31" and "MAIN/SNOMEDCT-UK/2017-07-31"

  stopifnot(length(branch) == 1)
  stopifnot(!is.na(branch))
  stopifnot(is.character(branch))
  stopifnot(branch != "")
  if ( !grepl("^MAIN", branch) ) {
    stop("snowstorm branches must begin with `MAIN`")
  }
  utils::URLencode(URL = branch, reserved = TRUE)
}



#' Deduplicate, clean and sort SNOMED CT identifiers
#'
#' @param x a vector of identifiers, such as SNOMED CT concept IDs
#'
#' @return a character vector of unique identifiers, without NA or empty strings.
#' @noRd
.snomed_identifiers_deduplicate <- function(x) {

  # remove NA and turn to character
  x <- trimws(as.character(stats::na.omit(x)))
  # remove empty strings
  x <- grep(pattern = "^$",
            x = x,
            value = TRUE, invert = TRUE)
  x <- sort(unique(x))

  x
}


#' Initiate progress bar
#'
#' @param x vector to determine the length of the progress bar
#' @param chunk_size integer interval size to determine the
#' length of the progress bar (for example, 10 will mean the progress
#' bar unit corresponds to chunks of 10 observations in vector \code{x})
#' @param silent if TRUE, returns a progress bar object, otherwise return
#' \code{NULL}
#'
#' @return an R6 object of class \code{progress_bar} if
#' \code{silent} is \code{TRUE}, \code{NULL} otherwise
#' @noRd
.progress_bar_initiate <- function(x, chunk_size, silent) {
  if (silent) {
    NULL
  } else {
    progress_bar <- progress::progress_bar$new(
      format = "  [:bar] :percent :eta",
      total = trunc(length(x)/chunk_size) + as.integer(length(x) %% chunk_size > 0)
    )
    progress_bar$tick(0)

    progress_bar
  }
}


#' Split a vector into a list of smaller vectors
#'
#' @param x a vector to split
#' @param max_length the maximum length of vectors to return
#'
#' @return a list of vector of length between 1 and \code{max_length}
#' @noRd
.split_into_chunks <- function(x, max_length){
  split(x, sort(trunc((seq_len(length(x)) - 1)/max_length)))
}

