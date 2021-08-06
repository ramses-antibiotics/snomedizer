## code to prepare `drug_indications` dataset goes here

library(dplyr)
drug_indications <- read.csv("data-raw/drug_indications.csv", stringsAsFactors = F)
drug_indications <- drug_indications %>%
  dplyr::transmute(
    prescription_id = 1:n(),
    indication
  )
usethis::use_data(drug_indications, overwrite = T)

