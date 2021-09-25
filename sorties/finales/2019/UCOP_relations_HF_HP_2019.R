library(tidyverse)
library(readxl)

# données Arches
identifiants_places <- read.csv(file = "EAMENA_2021-09-25_06-53-34.csv") %>%
  as_tibble()

identifiants_features <- read.csv(file = "identifiants_features_B8_to_10.csv") %>%
  as_tibble()

# données initiales de relations entre HF et HP
relations_features <- read_excel(path = "2019_relations_features_places.xlsx")

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
                     "UCOP_relations_heritage_features_and_places_2019_Arches_ID.xlsx",
                     append = TRUE)
