library(tidyverse)
library(readxl)

# données Arches
identifiants_features <- read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-38-18.csv") %>%
  as_tibble() %>%
  bind_rows(read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-39-40.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-42-09.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-43-44.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-46-23.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-48-27.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-52-50.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-54-02.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "HF_ID_IDIHA/EAMENA_2021-09-25_05-55-06.csv") %>%
              as_tibble()) %>%
  unique()

write.csv(x = identifiants_features, file = "identifiants_features_B8_to_10.csv")

identifiants_photos_sketches <- read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-25_06-01-54.csv") %>%
  as_tibble() %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-25_06-06-42.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-25_06-10-56.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-25_06-14-37.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-25_06-18-59.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-25_06-24-51.csv") %>%
              as_tibble()) %>%
  unique()

write.csv(x = identifiants_photos_sketches, file = "identifiants_photos_sketches_B8_to_10.csv", row.names = FALSE)

# données initiales de relations photos & features
# code is OK without P_number...
relations_photos <- read_excel(path = "UCOP_relations_photos_features_places_bulk_number_8.xlsx")

relations_photos <- relations_photos %>%
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

relations_photos <- relations_photos %>% 
  filter(!is.na(RESOURCEID_TO))

# sortie
relations_photos_liste <- list("RELATIONS" = relations_photos)
openxlsx::write.xlsx(relations_photos_liste,
                     "UCOP_relations_photos_features_places_bulk_8_Arches_ID.xlsx",
                     append = TRUE)

# 
# # données initiales de relations sketech & features
# relations_sketches <- read_excel(path = "UCOP_relations_photos_features_places_bulk_number_8.xlsx")
# 
# relations_sketches <- relations_sketches %>%
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
# relations_sketches <- relations_sketches %>% 
#   filter(!is.na(RESOURCEID_TO))
# 
# # sortie
# relations_sketches_liste <- list("RELATIONS" = relations_sketches)
# openxlsx::write.xlsx(relations_sketches_liste,
#                      "UCOP_relations_sketches_features_places_bulk_2_Arches_ID.xlsx",
#                      append = TRUE)
