.onLoad <- function(libname, pkgname) {

  # get any environment variables

  snomedizer_endpoint <- Sys.getenv("SNOMEDIZER_ENDPOINT")
  if ( snomedizer_endpoint == "" |
       length(snomedizer_endpoint) > 1 |
       is.character(snomedizer_endpoint) == FALSE) {
    snomedizer_endpoint <- snomed_public_endpoint_suggest()
  }

  snomedizer_branch <- Sys.getenv("SNOMEDIZER_BRANCH")
  if ( snomedizer_branch == "" |
       length(snomedizer_branch) > 1 |
       is.character(snomedizer_branch) == FALSE) {
    snomedizer_branch <- "MAIN"
  }

  snomedizer_limit <- Sys.getenv("SNOMEDIZER_LIMIT")
  if ( snomedizer_limit == "" |
       length(snomedizer_limit) > 1 |
       is.integer(snomedizer_limit) == FALSE) {
    snomedizer_limit <- 50
  }

  options(
    list(
      snomedizer.endpoint = snomedizer_endpoint,
      snomedizer.branch = snomedizer_branch,
      snomedizer.limit = snomedizer_limit,
      snomedizer.language = "en-X-900000000000509007,en-X-900000000000508004,en"
      )
  )

  invisible()
}
