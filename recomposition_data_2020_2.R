library(readxl)
library(openxlsx)
library(tidyverse)

data_2020_2 <- read_excel(path = "data/BDD_OS_2020_2_v2.xlsx")

significance_2020_2 <- read_excel(path = "data/Feature_significance_2020_2.xlsx")

condition_assessment_2020_2 <- read_excel(path = "data/2021_08_13_condition_assessment_2020_2_compilation.xlsx")

significance_2020_2 <- significance_2020_2 %>%
  select(`Feature ID`, `RCU_Feature Significance`) %>%
  mutate(`RCU_Feature Significance` = str_to_title(`RCU_Feature Significance`)) %>%
  unique()

condition_assessment_2020_2 <- condition_assessment_2020_2 %>%
  select(`Feature ID`, disturbance_causes, disturbance_categories, disturbance_effects) %>%
  unique()

condition_assessment_2020_2 %>%
  mutate(duplicata = duplicated(x = `Feature ID`)) %>%
  filter(duplicata == TRUE)

data_2020_2 %>%
  left_join(x = ., y = significance_2020_2, by = c("OS_Number" = "Feature ID")) %>%
  left_join(x = ., y = condition_assessment_2020_2, by = c("OS_Number" = "Feature ID")) -> data_2020_2_revu

list_of_dataset <- list("DATA" = data_2020_2_revu)
write.xlsx(list_of_dataset, 
           file = "data/BDD_OS_2020_2_v3.xlsx", 
           append = TRUE)


#### des éléments qui doivent être revus ####
data_2020_2 <- read_excel(path = "data/BDD_OS_2020_2_v3.xlsx")

data_2020_2 %>% select(`Main periods`) %>% unique()

data_2020_2 %>%
  mutate(`Description date` = as.character(`Description date`)) %>%
  mutate(`Site accessibility` = str_to_title(`Site accessibility`)) %>%
  mutate(`Main periods` = case_when(
    `Main periods` == "Late Ottoman|Modern" ~ "Islamic|Modern",
    `Main periods`== "Late Ottoman|Kingdom of Saudi Arabia" ~ "Islamic|Modern",
    `Main periods`== "Late Ottoman" ~ "Islamic",
    `Main periods`== "unknown" ~ "Unknown",
    `Main periods` == "Late Ottoman|Contemporary|Late Ottoman|Modern|WWI|Kingdom of Saudi Arabia|WWII|Modern" ~ "Islamic|Modern",
    `Main periods` == "Late Ottoman|Modern|WWI|Kingdom of Saudi Arabia|WWII|Modern" ~ "Islamic|Modern",
    `Main periods`== "Contemporary|Late Ottoman|Modern|WWI|Kingdom of Saudi Arabia|WWII|Modern" ~ "Islamic|Modern",
    TRUE ~ `Main periods`
  )) %>%
  mutate(Periods = case_when(
    `Main periods` == "Islamic|Modern" ~ "Late Ottoman|Unknown",
    `Main periods` == "Islamic" ~ "Late Ottoman",
    `Main periods` == "Modern" ~ "Unknown",
    `Main periods` == "Unknown" ~ "Unknown",
    `Main periods` == "unknown|Dadanite" ~ "Unknown|Dadanite",
    `Main periods` == "Nabataean Kingdom" ~ "Nabataean Kingdom",
    `Main periods` == "Dadanite" ~ "Dadanite"
  )) %>%
  mutate(`Main periods` = case_when(
    `Main periods` == "unknown|Dadanite" ~ "Unknown|Iron Age/Pre-Classical",
    `Main periods` == "Dadanite" ~ "Iron Age/Pre-Classical",
    `Main periods` == "Pre-Islamic" ~ "Classical/Pre-Islamic",
    `Main periods` == "Nabataean Kingdom" ~ "Classical/Pre-Islamic",
    TRUE ~ `Main periods`
  )) %>% 
  mutate(Periods = if_else(is.na(Periods), "Unknown", Periods)) %>%
  mutate(`Threat Levels` = str_to_title(`Threat Levels`)) %>%
  mutate(Description = case_when(
    !is.na(`Description TH`) ~ `Description TH`,
    is.na(`Description TH`) & !is.na(`Description Corrigée`) ~ `Description Corrigée`,
    TRUE ~ Description
  )) %>%
  mutate(Description = if_else(is.na(Description), "x", Description)) -> data_2020_2







