.onAttach <- function(libname, pkgname) {

  # get any environment variables

  snomedizer_endpoint <- Sys.getenv("SNOMEDIZER_ENDPOINT")
  if ( snomedizer_endpoint == "" |
       length(snomedizer_endpoint) > 1 |
       is.character(snomedizer_endpoint) == FALSE) {
    snomedizer_endpoint <- snomed_public_endpoint_suggest()
    if (!is.null(snomedizer_endpoint)) {
      packageStartupMessage(paste(
        "The following SNOMED CT Terminology Server has been selected:",
        snomedizer_endpoint,
        "This server may be used for reference purposes only.",
        "It MUST NOT be used in production. Please refer to ?snomedizer for details.",
        sep = "\n"
      ))
    }

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
      snomedizer.limit = snomedizer_limit
      )
  )

  invisible()
}
