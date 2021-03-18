library(tidyverse)
library(readxl)
library(sf)
library(openxlsx)

# sources fonctions
source("bulk_functions.R")


# general database
ucop_data_2019_2020_1 <- read_excel(path = "data/ucop_data_2019_2020_1_v3.xlsx")

# sélection des 500 premières features (versement par blocs)
ucop_data_500 <- ucop_data_2019_2020_1 %>%
  slice(1:500)

# dernière maj des données SIG, envoyées par VB (au 15/01/2019)
donnees_sig <- read_sf(dsn = "data/2021_01_features_general/export_2020_2.shp", 
                       stringsAsFactors = FALSE) %>%
  st_set_crs(value = 32637)


#### Lien SIG (poly)/BDD ####
ucop_data_500 %>% 
  select(OS_Number) %>% 
  unique() %>%
  nrow()
# on a bien 500 uniques en id


verif_bdd_sig <- donnees_sig %>%
  left_join(x = ., y = ucop_data_500 %>% 
              mutate(ajout = "existe_bdd"), by = c("FEATURE_ID" = "OS_Number"))

verif_bdd_sig %>%
  filter(!is.na(ajout)) %>%
  select(FEATURE_ID) %>%
  st_drop_geometry() %>%
  unique() %>%
  nrow()
# if less than unique(bdd)...



# then quelles entités sont absentes ?
entite_sig <- verif_bdd_sig %>%
  filter(!is.na(ajout)) %>%
  select(FEATURE_ID) %>%
  st_drop_geometry() %>%
  unique() %>%
  mutate(extract_number = str_sub(string = FEATURE_ID, start = 4, end = 9)) %>%
  arrange(extract_number)

entite_bdd <- ucop_data_500 %>% 
  select(OS_Number) %>%
  mutate(liste_perso = str_sub(string = OS_Number, start = 4, end = 9)) %>%
  select(liste_perso)


# id des entités manquantes
entite_bdd %>%
  left_join(., y = entite_sig, by = c("liste_perso" = "extract_number")) %>%
  filter(is.na(FEATURE_ID))

# qu'est-ce que c'est comme type d'entités ?
absence <- ucop_data_2019_2020_1 %>%
  left_join(., y = entite_bdd %>%
              left_join(., y = entite_sig, by = c("liste_perso" = "extract_number")) %>%
              filter(is.na(FEATURE_ID)) %>%
              mutate(identifiant = str_c("OS_", liste_perso, sep = "")) %>%
              mutate(blob = "blob"), by = c("OS_Number" = "identifiant")) %>%
  filter(blob == "blob")

openxlsx::write.xlsx(absence, file = "sorties/intermediaires/pas_sig.xlsx", append = TRUE)

