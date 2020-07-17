

#' Find SNOMED-CT concepts
#'
#' @description A wrapper function searching for a term or a list of
#' concepts
#' @param term a string containing search terms
#' @param conceptIds a character vector of one or more SNOMED-CT codes
#' to search
#' @param ecl a character expression contraint. Consult the
#' \href{http://snomed.org/ecl}{Expression Constraint Language guide}
#' for more detail.
#' @param activeFilter whether to restrict results to active concepts. Default is `TRUE`.
#' Consult the \href{http://snomed.org/gl}{SNOMED glossary} for more detail.
#' @param silent whether to hide warnings. Default is `FALSE`
#' @param ... other optional arguments listed in \code{\link{api_operations}}
#' @return a data frame
#' @export
#'
#' @examples
#' # Free text search
#' concepts_find("asthma")
#'
#' # Retrieve multiple concepts
#' concepts_find(conceptIds =  c("233604007", "68566005"))
#'
#' # Use the SNOMED-CT Expression Constraint Language
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

  x <- api_find_concepts(
    term = term,
    conceptIds = conceptIds,
    ecl = ecl,
    activeFilter = activeFilter,
    ...
  )
  ignore <- result_completeness(x, silent = silent)

  result_flatten(x)
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
#'
#' @examples
#' result_completeness(api_find_concepts(term = "pneumonia", limit = 10))
result_completeness <- function(x, silent = FALSE) {
  stopifnot(methods::is(x, "response"))
  complete <- httr::content(x)$total <= httr::content(x)$limit

  if(!complete & !silent) {
    warning(paste0(
      "This server request returned just ", httr::content(x)$limit,
      " of a total ", httr::content(x)$total, " results.\n",
      "Please increase the server limit."
    ))
  }

  complete
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
