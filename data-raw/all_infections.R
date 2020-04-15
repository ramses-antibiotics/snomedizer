
all_infections <- snowstorm_fetch_infections()
usethis::use_data(all_infections, internal = F, overwrite = T)
