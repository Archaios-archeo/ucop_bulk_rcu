library(tidyverse)
library(readxl)

# données Arches
identifiants_features <- read.csv(file = "identifiants_features_B8_to_10.csv") %>%
  as_tibble()

identifiants_photos_sketches <- read.csv(file = "identifiants_photos_sketches_B8_to_10.csv") %>%
  as_tibble()

# données initiales de relations photos & features
# # code is OK without P_number...
# relations_photos <- read_excel(path = "UCOP_relations_photos_features_places_bulk_number_10.xlsx")
# 
# relations_photos <- relations_photos %>%
#   left_join(x = ., y = identifiants_features %>%
#               mutate(nouveau = if_else(nchar(NAME.E41) > 8, "nouveau", "ancien")) %>%
#               mutate(NAME.E41 = str_sub(string = NAME.E41, start = 1, end = 8)) %>%
#               select(ARCHES.ID, NAME.E41),
#             by = c("RESOURCEID_TO" = "NAME.E41")) %>%
#   select(-RESOURCEID_TO) %>%
#   rename(RESOURCEID_TO = ARCHES.ID) %>%
#   left_join(x = ., y = identifiants_photos_sketches %>%
#               select(ARCHES.ID, CATALOGUE_ID.E42),
#             by = c("RESOURCEID_FROM" = "CATALOGUE_ID.E42")) %>%
#   select(-RESOURCEID_FROM) %>%
#   rename(RESOURCEID_FROM = ARCHES.ID) %>%
#   relocate(RESOURCEID_TO, .before = "START_DATE") %>%
#   relocate(RESOURCEID_FROM, .before = "RESOURCEID_TO")
# 
# relations_photos <- relations_photos %>% 
#   filter(!is.na(RESOURCEID_TO))
# 
# # sortie
# relations_photos_liste <- list("RELATIONS" = relations_photos)
# openxlsx::write.xlsx(relations_photos_liste,
#                      "UCOP_relations_photos_features_places_bulk_10_Arches_ID.xlsx",
#                      append = TRUE)

# 
# données initiales de relations sketech & features
relations_sketches <- read_excel(path = "UCOP_relations_sketches_features_places_2019_2020_1.xlsx")

relations_sketches <- relations_sketches %>%
  left_join(x = ., y = identifiants_features %>%
              mutate(nouveau = if_else(nchar(NAME.E41) > 8, "nouveau", "ancien")) %>%
              mutate(NAME.E41 = str_sub(string = NAME.E41, start = 1, end = 8)) %>%
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

relations_sketches <- relations_sketches %>%
  filter(!is.na(RESOURCEID_TO))

# sortie
relations_sketches_liste <- list("RELATIONS" = relations_sketches)
openxlsx::write.xlsx(relations_sketches_liste,
                     "UCOP_relations_sketches_features_places_bulk_2_to_10_Arches_ID.xlsx",
                     append = TRUE)
