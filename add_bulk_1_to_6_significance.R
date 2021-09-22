library(tidyverse)
library(readxl)
library(openxlsx)

data_2019_200_1 <- read_excel(path = "data/ucop_data_2019_2020_1_v5.xlsx")

# Bulk number 1
bulk_1_part_1 <- read_excel(path = "sorties/finales/500_features_bulk_1/UCOP_heritage_features_bulk_1_part_1_update.xlsx",
                                    sheet = "NameGroup")
bulk_1_part_1 <- bulk_1_part_1 %>%
  bind_cols(read_excel(path = "sorties/finales/500_features_bulk_1/UCOP_heritage_features_bulk_1_part_1_update.xlsx",
                       sheet = "DescriptionGroup"))

bulk_1_part_1 %>%
  left_join(x = ., y = data_2019_200_1 %>%
              select(OS_Number, `Feature significance RCU`),
            by = c("NAME.E41" = "OS_Number")) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = if_else(
    condition = !is.na(`Feature significance RCU`), true = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`),
    paste0(GENERAL_DESCRIPTION.E62, "|", "x")
  )) %>%
  select(NAME.E41, GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62) -> bulk_1_part_1

write.xlsx(x = bulk_1_part_1, file = "sorties/finales/500_features_bulk_1/significance_bulk_1_part_1.xlsx")

# Bulk number 1 : part 2
bulk_1_part_2 <- read_excel(path = "sorties/finales/500_features_bulk_1/UCOP_heritage_features_bulk_1_part_2_update.xlsx",
                            sheet = "NameGroup")
bulk_1_part_2 <- bulk_1_part_2 %>%
  bind_cols(read_excel(path = "sorties/finales/500_features_bulk_1/UCOP_heritage_features_bulk_1_part_2_update.xlsx",
                       sheet = "DescriptionGroup"))

bulk_1_part_2 %>%
  left_join(x = ., y = data_2019_200_1 %>%
              select(OS_Number, `Feature significance RCU`),
            by = c("NAME.E41" = "OS_Number")) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = if_else(
    condition = !is.na(`Feature significance RCU`), true = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`),
    paste0(GENERAL_DESCRIPTION.E62, "|", "x")
  )) %>%
  select(NAME.E41, GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62) -> bulk_1_part_2

write.xlsx(x = bulk_1_part_2, file = "sorties/finales/500_features_bulk_1/significance_bulk_1_part_2.xlsx")



# Bulk number 2
bulk_2 <- read_excel(path = "sorties/finales/500_features_bulk_2/UCOP_heritage_features_bulk_number_2.xlsx",
                            sheet = "NameGroup")
bulk_2 <- bulk_2 %>%
  bind_cols(read_excel(path = "sorties/finales/500_features_bulk_2/UCOP_heritage_features_bulk_number_2.xlsx",
                       sheet = "DescriptionGroup"))

bulk_2 %>%
  left_join(x = ., y = data_2019_200_1 %>%
              select(OS_Number, `Feature significance RCU`),
            by = c("NAME.E41" = "OS_Number")) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = if_else(
    condition = !is.na(`Feature significance RCU`), true = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`),
    paste0(GENERAL_DESCRIPTION.E62, "|", "x")
  )) %>%
  select(NAME.E41, GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62) -> bulk_2

write.xlsx(x = bulk_2, file = "sorties/finales/500_features_bulk_2/significance_bulk_2.xlsx")



# Bulk number 3
bulk_3 <- read_excel(path = "sorties/finales/500_features_bulk_3/UCOP_heritage_features_bulk_number_3.xlsx",
                     sheet = "NameGroup")
bulk_3 <- bulk_3 %>%
  bind_cols(read_excel(path = "sorties/finales/500_features_bulk_3/UCOP_heritage_features_bulk_number_3.xlsx",
                       sheet = "DescriptionGroup"))

bulk_3 %>%
  left_join(x = ., y = data_2019_200_1 %>%
              select(OS_Number, `Feature significance RCU`),
            by = c("NAME.E41" = "OS_Number")) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = if_else(
    condition = !is.na(`Feature significance RCU`), true = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`),
    paste0(GENERAL_DESCRIPTION.E62, "|", "x")
  )) %>%
  select(NAME.E41, GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62) -> bulk_3

write.xlsx(x = bulk_3, file = "sorties/finales/500_features_bulk_3/significance_bulk_3.xlsx")



# Bulk number 4
bulk_4 <- read_excel(path = "sorties/finales/500_features_bulk_4/UCOP_heritage_features_bulk_number_4.xlsx",
                     sheet = "NameGroup")
bulk_4 <- bulk_4 %>%
  bind_cols(read_excel(path = "sorties/finales/500_features_bulk_4/UCOP_heritage_features_bulk_number_4.xlsx",
                       sheet = "DescriptionGroup"))

bulk_4 %>%
  left_join(x = ., y = data_2019_200_1 %>%
              select(OS_Number, `Feature significance RCU`),
            by = c("NAME.E41" = "OS_Number")) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = if_else(
    condition = !is.na(`Feature significance RCU`), true = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`),
    paste0(GENERAL_DESCRIPTION.E62, "|", "x")
  )) %>%
  select(NAME.E41, GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62) -> bulk_4

write.xlsx(x = bulk_4, file = "sorties/finales/500_features_bulk_4/significance_bulk_4.xlsx")


# Bulk number 5
bulk_5 <- read_excel(path = "sorties/finales/500_features_bulk_5/UCOP_heritage_features_bulk_number_5.xlsx",
                     sheet = "NameGroup")
bulk_5 <- bulk_5 %>%
  bind_cols(read_excel(path = "sorties/finales/500_features_bulk_5/UCOP_heritage_features_bulk_number_5.xlsx",
                       sheet = "DescriptionGroup"))

bulk_5 %>%
  mutate(OS_Number = str_sub(string = NAME.E41, start = 1, end = 8)) %>%
  left_join(x = ., y = data_2019_200_1 %>%
              select(OS_Number, `Feature significance RCU`),
            by = "OS_Number") %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = if_else(
    condition = !is.na(`Feature significance RCU`), true = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`),
    paste0(GENERAL_DESCRIPTION.E62, "|", "x")
  )) %>%
  select(NAME.E41, GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62) -> bulk_5

write.xlsx(x = bulk_5, file = "sorties/finales/500_features_bulk_5/significance_bulk_5.xlsx")

# Bulk number 6
bulk_6 <- read_excel(path = "sorties/finales/500_features_bulk_6/UCOP_heritage_features_bulk_number_6.xlsx",
                     sheet = "NameGroup")
bulk_6 <- bulk_6 %>%
  bind_cols(read_excel(path = "sorties/finales/500_features_bulk_6/UCOP_heritage_features_bulk_number_6.xlsx",
                       sheet = "DescriptionGroup"))

bulk_6 %>%
  mutate(OS_Number = str_sub(string = NAME.E41, start = 1, end = 8)) %>%
  left_join(x = ., y = data_2019_200_1 %>%
              select(OS_Number, `Feature significance RCU`),
            by = "OS_Number") %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = if_else(
    condition = !is.na(`Feature significance RCU`), true = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`),
    paste0(GENERAL_DESCRIPTION.E62, "|", "x")
  )) %>%
  select(NAME.E41, GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62) -> bulk_6

write.xlsx(x = bulk_6, file = "sorties/finales/500_features_bulk_6/significance_bulk_6.xlsx")

