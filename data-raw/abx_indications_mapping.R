

devtools::load_all()
library(dplyr)
inf_concepts <- concepts_descendants(conceptIds = "40733004",
                                     direct_descendants = FALSE,
                                     activeFilter = TRUE,
                                     limit = 10000)[[1]]
# too large a query
# inf_concepts$descriptions <- concepts_descriptions(conceptIds = inf_concepts$conceptId)
pneumo_uti_concepts <- concepts_descendants(conceptIds = c("233604007", "68566005"),
                                            direct_descendants = FALSE,
                                            activeFilter = TRUE,
                                            limit = 10000)
pneumo_uti_concepts <- dplyr::bind_rows(pneumo_uti_concepts)

api_descriptions()
pneumo_uti_concepts$descriptions <- concepts_descriptions(conceptIds = pneumo_uti_concepts$conceptId)
save(pneumo_uti_concepts, file = "data-raw/pneumo_uti_concepts.rda")
save(inf_concepts, file = "data-raw/inf_concepts.rda")


##################

devtools::load_all()
library(dplyr)
library(stringr)
load("data-raw/pneumo_uti_concepts.rda")
load("data-raw/inf_concepts.rda")


inf_descriptions <- pneumo_uti_concepts$descriptions %>%
  dplyr::bind_rows() %>%
  dplyr::filter(active, type != "FSN") %>%
  dplyr::transmute(conceptId, descriptionText = term)
inf_descriptions <- dplyr::bind_rows(
  inf_descriptions,
  dplyr::transmute(inf_concepts, conceptId, descriptionText = pt.term))
inf_descriptions <- dplyr::distinct(inf_descriptions)


inf_descriptions <- dplyr::mutate(
  inf_descriptions,
  tokens_concept = stringr::str_split(tolower(descriptionText), " ")
)
unique_tokens_concepts <- tibble(tokens = unlist(inf_descriptions$tokens_concept)) %>%
  group_by(tokens) %>%
  summarise(n = n()) %>%
  arrange(-n)
inf_descriptions <- mutate(inf_descriptions, tokens_concept = lapply(tokens_concept, function(x) {
  x[!x %in% c("as", "per", "micro", "to", "in", "of", "and", "due", "caused",
              "for", "on", "with", "by", "from", "-", "/", "and/or", "the", "a", "or")]
}))


rx_indications <- read.csv("data-raw/abx_indications.csv", stringsAsFactors = F) %>%
  mutate(indication_string = str_remove(tolower(indication), "[?]")) %>%
  mutate(indication_string = str_replace(indication_string, "( )+", " ")) %>%
  mutate(tokens_src = stringr::str_split(trimws(indication_string), "( )|(/)"),
         as_per_micro = grepl(
           "(as per micro)|(d/w micro)|(micro advice)|(discussed with micro)|(micro approved)",
           indication_string))

unique_tokens <- tibble(tokens = unlist(rx_indications$tokens_src)) %>%
  group_by(tokens) %>%
  summarise(n = n()) %>%
  arrange(-n)

rx_indications <- rx_indications %>%
  mutate(tokens_src = lapply(tokens_src, function(x) {
    x[!x %in% c("as", "per", "micro", "to", "in", "of", "and", "due", "caused",
                "for", "on", "with", "by", "from", "-", "/", "and/or", "the", "a", "or")]
  }))


concept_mapping <- tidyr::crossing(rx_indications, inf_descriptions)

concept_mapping <- mutate(
  concept_mapping,
  overlap_tot = purrr::map2_int(tokens_src, tokens_concept, function(source, concept) {
    sum(concept %in% source)
  }),
  overlap_pct = purrr::map2_dbl(tokens_src, tokens_concept, function(source, concept) {
    sum(concept %in% source)/length(concept)
  }))

concept_mapping %>%
  filter(overlap_pct > .1) %>% View()

concept_mapping  %>% group_by(indication) %>%
  arrange(-overlap_pct, -overlap_tot) %>%
  mutate(rank= 1:n()) %>%
  filter(rank <= 5) %>%
  ungroup() %>%
  View()

