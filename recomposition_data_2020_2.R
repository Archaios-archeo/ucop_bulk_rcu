library(readxl)
library(openxlsx)
library(tidyverse)

data_2020_2 <- read_excel(path = "data/BDD_OS_2020_2.xlsx")

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
  left_join(x = ., y = significance_2020_2, by = "Feature ID") %>%
  left_join(x = ., y = condition_assessment_2020_2, by = "Feature ID") -> data_2020_2_revu

list_of_dataset <- list("DATA" = data_2020_2)
write.xlsx(list_of_dataset, 
           file = "data/BDD_OS_2020_2_v2.xlsx", 
           append = TRUE)
