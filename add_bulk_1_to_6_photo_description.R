library(tidyverse)
library(readxl)
library(openxlsx)

# data
data_2019_2020_1 <- read_excel(path = "data/ucop_data_2019_2020_1_v4.xlsx")


# bulk 1 : photos
photos_bulk_1 <- read_excel(path = "sorties/finales/500_features_bulk_1/UCOP_ressources_photos_bulk_1.xlsx", sheet = "NOT")

photos_bulk_1 %>%
  mutate(OS_Number = str_sub(string = CATALOGUE_ID.E42, start = 1, end = 8)) %>%
  left_join(., y = data_2019_2020_1 %>%
              select(OS_Number, `Feature form`, featInterpretationType)) %>%
  mutate(DESCRIPTION.E62 = paste0("photo of a ", str_to_lower(`Feature form`), ": ", str_to_lower(featInterpretationType))) -> photos_bulk_1

write.xlsx(x = photos_bulk_1, file = "sorties/finales/500_features_bulk_1/photos_description_bulk_1.xlsx")

# bulk 2 : photos
photos_bulk_2 <- read_excel(path = "sorties/finales/500_features_bulk_2/UCOP_ressources_photos_bulk_number_2.xlsx", sheet = "NOT")

photos_bulk_2 %>%
  mutate(OS_Number = str_sub(string = CATALOGUE_ID.E42, start = 1, end = 8)) %>%
  left_join(., y = data_2019_2020_1 %>%
              select(OS_Number, `Feature form`, featInterpretationType)) %>%
  mutate(DESCRIPTION.E62 = paste0("photo of a ", str_to_lower(`Feature form`), ": ", str_to_lower(featInterpretationType))) -> photos_bulk_2

write.xlsx(x = photos_bulk_2, file = "sorties/finales/500_features_bulk_2/photos_description_bulk_2.xlsx")

# bulk 3 : photos
photos_bulk_3 <- read_excel(path = "sorties/finales/500_features_bulk_3/UCOP_ressources_photos_number_3.xlsx", sheet = "NOT")

photos_bulk_3 %>%
  mutate(OS_Number = str_sub(string = CATALOGUE_ID.E42, start = 1, end = 8)) %>%
  left_join(., y = data_2019_2020_1 %>%
              select(OS_Number, `Feature form`, featInterpretationType)) %>%
  mutate(DESCRIPTION.E62 = paste0("photo of a ", str_to_lower(`Feature form`), ": ", str_to_lower(featInterpretationType))) -> photos_bulk_3

write.xlsx(x = photos_bulk_3, file = "sorties/finales/500_features_bulk_3/photos_description_bulk_3.xlsx")


# bulk 4 : photos
photos_bulk_4 <- read_excel(path = "sorties/finales/500_features_bulk_4/UCOP_ressources_photos_number_4.xlsx", sheet = "NOT")

photos_bulk_4 %>%
  mutate(OS_Number = str_sub(string = CATALOGUE_ID.E42, start = 1, end = 8)) %>%
  left_join(., y = data_2019_2020_1 %>%
              select(OS_Number, `Feature form`, featInterpretationType)) %>%
  mutate(DESCRIPTION.E62 = paste0("photo of a ", str_to_lower(`Feature form`), ": ", str_to_lower(featInterpretationType))) -> photos_bulk_4

write.xlsx(x = photos_bulk_4, file = "sorties/finales/500_features_bulk_4/photos_description_bulk_4.xlsx")
