library(tidyverse)
library(readxl)

#### Data source ####
heritage_places_2019_2020_1 <- read_excel(path = "UCOP_heritage_places_2019_update.xlsx", sheet = "NameGroup") %>%
  bind_rows(read_excel(path = "UCOP_heritage_places_2020_1_compilation.xlsx", sheet = "NameGroup"))

heritage_places_2019_2020_1 %>%
  select(NAME.E41) %>%
  mutate(heritage_place = "oui") -> heritage_places_2019_2020_1

#### Relations photographies et heritage places ####
compilation_relations_bulk_1_to_10 <- read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_1.xlsx") %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_10.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_2.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_3.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_4.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_5.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_6.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_7.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_8.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_features_places_bulk_number_9.xlsx")) %>%
  bind_rows(read_excel(path = "photos/UCOP_relations_photos_places_2020_1.xlsx"))

compilation_relations_bulk_1_to_10 %>%
  left_join(x = ., y = heritage_places_2019_2020_1, by = c("RESOURCEID_TO" = "NAME.E41")) %>%
  filter(!is.na(heritage_place)) %>%
  select(-heritage_place) %>%
  mutate(NOTES = paste0("Photo of ", RESOURCEID_TO)) %>%
  unique()-> compilation_relations_bulk_1_to_10

# sortie
relations_photos_liste <- list("RELATIONS" = compilation_relations_bulk_1_to_10)
openxlsx::write.xlsx(relations_photos_liste,
                     "UCOP_relations_photos_places_2019_2020_1.xlsx",
                     append = TRUE)


#### Adding Arches ID ####
# données Arches
identifiants_places <- read.csv(file = "EAMENA_2021-09-25_06-53-34.csv") %>%
  as_tibble() %>%
  unique()

identifiants_photos_sketches <- read.csv(file = "identifiants_photos_sketches_B8_to_10.csv") %>%
  as_tibble() %>%
  unique()


# données initiales de relations photos & features
relations_photos <- compilation_relations_bulk_1_to_10 %>%
  left_join(x = ., y = identifiants_places %>%
              select(ARCHES.ID, NAME.E41),
            by = c("RESOURCEID_TO" = "NAME.E41")) %>%
  select(-RESOURCEID_TO) %>%
  rename(RESOURCEID_TO = ARCHES.ID) %>%
  left_join(x = ., y = identifiants_photos_sketches %>%
              select(ARCHES.ID, CATALOGUE_ID.E42),
            by = c("RESOURCEID_FROM" = "CATALOGUE_ID.E42")) %>%
  select(-RESOURCEID_FROM) %>%
  rename(RESOURCEID_FROM = ARCHES.ID) %>%
  relocate(RESOURCEID_TO, .before = "START_DATE") %>%
  relocate(RESOURCEID_FROM, .before = "RESOURCEID_TO")

relations_photos <- relations_photos %>% 
  filter(!is.na(RESOURCEID_TO))

# sortie
relations_photos_liste <- list("RELATIONS" = relations_photos)
openxlsx::write.xlsx(relations_photos_liste,
                     "UCOP_relations_photos_features_places_bulk_1_Arches_ID.xlsx",
                     append = TRUE)



#### Relations sketches et heritage places ####
compilation_relations_sktches <- read_excel(path = "sketches/UCOP_relations_sketches_features_places_bulk_1.xlsx") %>%
  bind_rows(read_excel(path = "sketches/UCOP_relations_sketches_features_places_bulk_1.xlsx")) %>%
  bind_rows(read_excel(path = "sketches/UCOP_relations_sketches_places_2020_1.xlsx"))

compilation_relations_sktches %>%
  left_join(x = ., y = heritage_places_2019_2020_1, by = c("RESOURCEID_TO" = "NAME.E41")) %>%
  filter(!is.na(heritage_place)) %>%
  select(-heritage_place) %>%
  mutate(NOTES = paste0("Sketch of ", RESOURCEID_TO)) %>%
  unique() -> compilation_relations_bulk_1_to_10

# sortie
relations_photos_liste <- list("RELATIONS" = compilation_relations_sktches)
openxlsx::write.xlsx(relations_photos_liste,
                     "UCOP_relations_sketches_places_2019_2020_1.xlsx",
                     append = TRUE)

