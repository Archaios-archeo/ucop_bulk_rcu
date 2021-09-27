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

identifiants_features <- read.csv(file = "identifiants_features_B8_to_10.csv") %>%
  as_tibble()

# données initiales de relations entre HF et HP
relations_features <- read_excel(path = "2020_1_relations_features_places_compilation.xlsx")

relations_features <- relations_features %>%
  left_join(x = ., y = identifiants_features %>%
              mutate(nouveau = if_else(nchar(NAME.E41) > 8, "nouveau", "ancien")) %>%
              mutate(NAME.E41 = str_sub(string = NAME.E41, start = 1, end = 8)) %>%
              select(ARCHES.ID, NAME.E41),
            by = c("RESOURCEID_TO" = "NAME.E41")) %>%
  select(-RESOURCEID_TO) %>%
  rename(RESOURCEID_TO = ARCHES.ID) %>%
  left_join(x = ., y = identifiants_places %>%
              select(ARCHES.ID, NAME.E41),
            by = c("RESOURCEID_FROM" = "NAME.E41")) %>%
  select(-RESOURCEID_FROM) %>%
  rename(RESOURCEID_FROM = ARCHES.ID) %>%
  relocate(RESOURCEID_TO, .before = "START_DATE") %>%
  relocate(RESOURCEID_FROM, .before = "RESOURCEID_TO")

relations_features <- relations_features %>%
  filter(!is.na(RESOURCEID_TO))

# sortie
relations_features_liste <- list("RELATIONS" = relations_features)
openxlsx::write.xlsx(relations_features_liste,
                     "UCOP_relations_heritage_features_and_places_2020_1_Arches_ID.xlsx",
                     append = TRUE)
