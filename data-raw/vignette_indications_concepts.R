
library(snomedizer)
library(dplyr)
snomedizer_options_set(
  endpoint = "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct",
  branch = "MAIN/2021-07-31",
  limit = 10000
)

abx_indications_concepts <- concepts_descendants(
  conceptIds = c("40733004 | Infectious disease (disorder) |",
                 "128139000 | Inflammatory disorder (disorder) |",
                 "128477000 | Abscess (disorder) |")
) %>%
  dplyr::bind_rows() %>%
  unique()

abx_indications_descriptions <- abx_indications_concepts$conceptId %>%
  concepts_descriptions() %>%
  dplyr::bind_rows()

usethis::use_data(abx_indications_concepts, abx_indications_descriptions, internal = TRUE, overwrite = TRUE)
