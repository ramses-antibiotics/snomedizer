
devtools::load_all()
library(dplyr)
local_endpoint <- "http://localhost:8080"
local_branch <- "MAIN"
snomed_endpoint_test(local_endpoint,
                     local_branch)
infection_concepts <- concepts_descendants(
  conceptIds = "40733004",
  direct_descendants = FALSE,
  activeFilter = TRUE,
  endpoint = local_endpoint,
  branch = local_branch,
  limit = 10000
)[[1]]


infection_child <- concepts_descendants(
  conceptIds = "40733004",
  direct_descendants = TRUE,
  activeFilter = TRUE,
  endpoint = local_endpoint,
  branch = local_branch,
  limit = 10000
)[[1]]

# Warning: takes 15minutes to run on local server
Sys.time()
infection_concepts$descriptions <- concepts_descriptions(conceptIds = infection_concepts$conceptId)
Sys.time()
save(infection_concepts, file = "data-raw/vignette_abx_indications/infection_concepts.rda")

usethis::use_data(infection_concepts, overwrite = TRUE, compress = "xz")
