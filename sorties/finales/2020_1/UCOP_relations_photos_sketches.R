library(tidyverse)
library(readxl)

# données Arches
identifiants_places <- read.csv(file = "HP_ID_IDIHA/EAMENA_2021-09-27_11-37-09.csv") %>%
  as_tibble() %>%
  select(PRIMARY.NAME:NAME.E41) %>%
  bind_rows(read.csv(file = "HP_ID_IDIHA/EAMENA_2021-09-27_11-37-48.csv") %>%
              as_tibble()  %>%
              select(PRIMARY.NAME:NAME.E41)) %>%
  bind_rows(read.csv(file = "HP_ID_IDIHA/EAMENA_2021-09-27_11-40-13.csv") %>%
              as_tibble()  %>%
              select(PRIMARY.NAME:NAME.E41)) %>%
  bind_rows(read.csv(file = "HP_ID_IDIHA/EAMENA_2021-09-27_11-41-08.csv") %>%
              as_tibble()  %>%
              select(PRIMARY.NAME:NAME.E41)) %>%
  bind_rows(read.csv(file = "HP_ID_IDIHA/EAMENA_2021-09-27_11-42-03.csv") %>%
              as_tibble()  %>%
              select(PRIMARY.NAME:NAME.E41)) %>%
  bind_rows(read.csv(file = "HP_ID_IDIHA/EAMENA_2021-09-27_11-44-10.csv") %>%
              as_tibble()  %>%
              select(PRIMARY.NAME:NAME.E41)) %>%
  unique()

identifiants_photos_sketches <- read.csv(file = "Photos_sketches_ID_IDIHA/identifiants_photos_sketches_B8_to_10.csv") %>%
  as_tibble() %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-27_11-48-27.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-27_11-50-32.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-27_11-52-17.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-27_12-08-00.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-27_12-08-50.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-27_12-09-35.csv") %>%
              as_tibble()) %>%
  bind_rows(read.csv(file = "Photos_sketches_ID_IDIHA/EAMENA_2021-09-27_12-10-23.csv") %>%
              as_tibble()) %>%
  unique()

# données initiales de relations photos & places
relations_photos <- read_excel(path = "UCOP_relations_photos_places_2020_1.xlsx")

relations_photos <- relations_photos %>%
  left_join(x = ., y = identifiants_places %>%
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
                     "UCOP_relations_photos_places_2020_1_Arches_ID.xlsx",
                     append = TRUE)

# 
# # données initiales de relations sketech & features
relations_sketches <- read_excel(path = "UCOP_relations_sketches_places_2019_2020_1.xlsx")

relations_sketches <- relations_sketches %>%
  left_join(x = ., y = identifiants_places %>%
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
                     "UCOP_relations_sketches_places_2020_1_Arches_ID.xlsx",
                     append = TRUE)
