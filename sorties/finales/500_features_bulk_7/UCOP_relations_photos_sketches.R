library(tidyverse)
library(readxl)

# données Arches
identifiants_features <- read.csv(file = "EAMENA_2021-09-23_04-18-29.csv") %>%
  as_tibble()

identifiants_photos_sketches <- read.csv(file = "EAMENA_2021-09-23_05-19-09.csv") %>%
  as_tibble() %>%
  bind_rows(read.csv(file = "EAMENA_2021-09-23_05-20-15.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "EAMENA_2021-09-23_05-25-51.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "EAMENA_2021-09-23_05-26-54.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "EAMENA_2021-09-23_05-28-01.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "EAMENA_2021-09-23_05-29-13.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "EAMENA_2021-09-23_05-31-47.csv") %>%
              as_tibble()) %>%
  unique()
  

# données initiales de relations photos & features
relations_photos <- read_excel(path = "UCOP_relations_photos_features_places_bulk_number_7.xlsx")

relations_photos <- relations_photos %>%
  left_join(x = ., y = identifiants_features %>%
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
  filter(!is.na(RESOURCEID_TO)) %>%
  filter(!is.na(RESOURCEID_FROM))

# sortie
relations_photos_liste <- list("RELATIONS" = relations_photos)
openxlsx::write.xlsx(relations_photos_liste,
                     "UCOP_relations_photos_features_places_bulk_7_Arches_ID.xlsx",
                     append = TRUE)


# code is OK without P_number...
# so, I need to add the relations for bulk 4
relations_photos <- read_excel(path = "UCOP_relations_photos_features_places_number_4.xlsx")

relations_photos <- relations_photos %>%
  left_join(x = ., y = identifiants_features %>%
              mutate(nouveau = if_else(nchar(NAME.E41) > 8, "nouveau", "ancien")) %>%
              mutate(NAME.E41 = str_sub(string = NAME.E41, start = 1, end = 8)) %>%
              filter(nouveau == "nouveau") %>%
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
                     "UCOP_relations_photos_features_places_bulk_4_Arches_ID_v2.xlsx",
                     append = TRUE)


# données initiales de relations sketech & features
relations_sketches <- read_excel(path = "UCOP_relations_sketches_features_places_bulk_1.xlsx")

relations_sketches <- relations_sketches %>%
  left_join(x = ., y = identifiants_features %>%
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
                     "UCOP_relations_sketches_features_places_bulk_1_Arches_ID.xlsx",
                     append = TRUE)
