
library(snomedizer)
snomedizer_options_set(
  endpoint = "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct",
  branch = "MAIN/2021-07-31",
  limit = 10000
)

abx_indications_concepts <- concepts_descendants(
  conceptIds = c("40733004 | Infectious disease (disorder) |",
                 "128045006 | Cellulitis (disorder) |",
                 "128477000 | Abscess (disorder) |")
) %>%
  bind_rows() %>%
  unique()

abx_indications_descriptions <- concepts_descriptions(
  abx_indications_concepts$conceptId
)
