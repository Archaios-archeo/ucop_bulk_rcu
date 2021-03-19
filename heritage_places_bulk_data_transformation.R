library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(naniar) # for replace_with_na
library(openxlsx) # pour la sortie du fichier excel
tmap_mode("view")

# sources fonctions
source("bulk_functions.R")


#### data sources ####
# general database
ucop_data_2019_2020_1 <- read_excel(path = "data/ucop_data_2019_2020_1_v3.xlsx")

# spatial data-compilation from heritage places spatial operations
# see http://github.com/Archaios-archeo/ucop_bulk_rcu/blob/main/spatial_operations_on_features_and_places.R
heritage_place_gis <- st_read("sorties/finales/2019/heritage_places_2019.gpkg")

heritage_place_tibble <- heritage_place_gis %>%
  st_drop_geometry() %>%
  left_join(., y = ucop_data_2019_2020_1, by = c("FEATURE_ID" = "OS_Number")) %>%
  mutate(length_ok = as.character(round(x = length_ok, digits = 2)),
         width_ok = as.character(round(x = width_ok, digits = 2))) %>%
  as_tibble()

RELATIONS <- read_excel(path = "sorties/finales/2019/2019_relations_features_places.xlsx")


#### création du fichier pour le "bulk apload" ####
# relation table between UCOP data colmun and IDIHA column :
lien_descripteurs <- read.csv("data/relations_rcu_idiha_ucop_features.csv", header = TRUE, stringsAsFactors = FALSE,
                              encoding = "UTF-8") %>%
  as_tibble()


## Réorganisation des données en entrée pour correspondre au différentes feuilles demandées par la RCU dans IDIHA
data_pivot <- heritage_place_tibble %>%
  select(-numero, -date_modif, -voie) %>%
  rowid_to_column("ID") %>% # pour faire un pivot_longer complet (avec toutes les colonnes initiales) ensuite
  pivot_longer(data = ., cols = -ID, 
               names_to = "ucop_real_name", 
               values_to = "valeurs_ucop_origine") %>% # pivot pour faire ensuite la jointure avec les noms de IDIHA
  left_join(., y = lien_descripteurs %>% # jointure avec le tableau de correspondance des noms IDIHA
              select(rcu_entite:if_yes_what), # sélection des éléments possiblement utiles, le reste étant perso, 
            by = "ucop_real_name") %>%
  filter(rcu_entite != "_Features")


#### création de toutes les "feuilles" (soit les dataframes) demandés par la RCU dans IDIHA ####
# on crée ici les choses par tableau, les uns après les autres pour ensuite sortir le .xlsx
# cela implique plus de lignes de codes mais plus facilement changeable et plus lisible pour le/la suivant.e

# Assessment : feuille demandée par la RCU
sortie_zzAssessment <- data_pivot %>%
  filter(rcu_onglet == "zzAssessment") %>% # sélection des infos à incorporer dans la future feuille zzAssessment
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  # recréation d'un tableau en format wide (soit un tableau élémentaire)
  select(-ID) %>% # on vire l'ID qui ne nous sert que pour les pivot_longer and wider mais pas une réelle info
  mutate(INVESTIGATOR_NAME.E41 = if_else(
    condition = is.na(INVESTIGATOR_NAME.E41),
    true = "UCOP Team",
    false = INVESTIGATOR_NAME.E41
  )) %>%  
  # si, dans certains cas, on n'a pas connaissance des personnes ayant enregistré les infos, on met le nom général du
  # projet Archaios au sein d'IDIHA
  mutate(INVESTIGATOR_NAME.E41 = str_c(INVESTIGATOR_NAME.E41, "UCOP Team", sep = "|")) %>% 
  # ajout systématique du nom du projet
  mutate(INVESTIGATOR_ROLE_TYPE.E55 = "IDIHA Project/RCU Staff") %>%
  # création de la colonne investigator qui est lié à un "pattern" systématique
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "INVESTIGATOR_NAME.E41", 
                             variable_revue = "INVESTIGATOR_ROLE_TYPE.E55") %>%
  # application de la fonction développée pour répéter les infos avec les pipes
  relocate(INVESTIGATOR_ROLE_TYPE.E55, .after = INVESTIGATOR_NAME.E41) %>%
  # réorganisation des colonnes, investigator role après les names
  relocate(ASSESSMENT_ACTIVITY_DATE.E49, .after = ASSESSMENT_ACTIVITY_TYPE.E55) %>%
  # idem
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "INVESTIGATOR_NAME.E41", 
                             variable_revue = "ASSESSMENT_ACTIVITY_TYPE.E55") %>% 
  # application fonction de répétition des informations
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "INVESTIGATOR_NAME.E41", 
                             variable_revue = "ASSESSMENT_ACTIVITY_DATE.E49")  %>% 
  # application fonction de répétition des informations
  mutate(INVESTIGATOR_NAME.E41 = str_c(INVESTIGATOR_NAME.E41, "Julie Gravier", sep = "|"),
         INVESTIGATOR_ROLE_TYPE.E55 = str_c(INVESTIGATOR_ROLE_TYPE.E55, "IDIHA Project/RCU Staff", sep = "|"),
         ASSESSMENT_ACTIVITY_TYPE.E55 = str_c(ASSESSMENT_ACTIVITY_TYPE.E55, "Desk-based (Unspecified)", sep = "|"),
         ASSESSMENT_ACTIVITY_DATE.E49 = str_c(ASSESSMENT_ACTIVITY_DATE.E49, Sys.Date(), sep = "|")) 
  # ajout systématique obligatoire de mon propre nom en tant que travail de desk-based, à la date du code, et en tant que membre IDIHA
  
  



# NameGroup : feuille demandée par la RCU
sortie_NameGroup <- data_pivot %>%
  filter(rcu_onglet == "NameGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(NAME_TYPE.E55 = "Alternative Reference")



# DescriptionGroup : feuille demandée par la RCU
sortie_DescriptionGroup <- data_pivot %>%
  filter(rcu_onglet == "DescriptionGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = "General Description") %>%
  relocate(GENERAL_DESCRIPTION.E62, .after = GENERAL_DESCRIPTION_TYPE.E55) %>%
  mutate(GENERAL_DESCRIPTION.E62 = if_else(
    is.na(GENERAL_DESCRIPTION.E62),
    "x",
    GENERAL_DESCRIPTION.E62
  ))



# AdminAreasGroup : feuille demandée par la RCU
sortie_AdminAreasGroup <- sortie_DescriptionGroup %>%
  rowid_to_column() %>%
  select(rowid) %>%
  mutate(ADMINISTRATIVE_DIVISION_NAME.E44 = "Al Ula",
         ADMINISTRATIVE_DIVISION_TYPE.E55 = "County/Governorate") %>%
  select(-rowid)



# MeasurementGroup : feuille demandée par la RCU. Concernant les HP, on en a pas
sortie_MeasurementGroup <- sortie_DescriptionGroup %>%
  rowid_to_column() %>%
  select(rowid) %>%
  mutate(MEASUREMENT_NUMBER.E60 = "x",
         MEASUREMENT_UNIT.E58 = "x",
         DIMENSION_TYPE.E55 = "x",
         MEASUREMENT_SOURCE_TYPE.E55 = "x") %>%
  select(-rowid)
# je pense qu'on peut la supprimer: à voir avec Laura


# PeriodGroup : feuille demandée par la RCU
# en fait, je n'ai pas encore travaillé à la gestion des périodes !!
# donc je le fais ci-dessus mais le code sera à virer pour plus tard
sortie_PeriodGroup <- data_pivot %>%
  filter(rcu_onglet == "PeriodGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  mutate(CULTURAL_PERIOD_CERTAINTY.I6 = case_when(
    CULTURAL_PERIOD_CERTAINTY.I6 == "Low" ~ "Possible",
    CULTURAL_PERIOD_CERTAINTY.I6 == "Medium" ~ "Probable",
    CULTURAL_PERIOD_CERTAINTY.I6 == "High" ~ "Definite",
    TRUE ~  CULTURAL_PERIOD_CERTAINTY.I6
  )) %>%
  select(-ID) %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "CULTURAL_PERIOD_CERTAINTY.I6") %>%
  relocate(CULTURAL_PERIOD_CERTAINTY.I6, .after = CULTURAL_PERIOD_DETAIL_TYPE.E55)


# FeatureGroup : feuille demandée par la RCU
sortie_FeatureGroup <- data_pivot %>%
  filter(rcu_onglet == "FeatureGroup" | rcu_onglet == "NameGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  left_join(x = ., y = RELATIONS %>%
              select(-START_DATE:-RELATION_TYPE), 
            by = c("NAME.E41" = "RESOURCEID_FROM")) %>%
  group_by_at(vars(-RESOURCEID_TO)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(FEATURE_NUMBER_TYPE.E55 = case_when(
    n == 1 ~ "1",
    n >= 2 & n <= 5 ~ "2 to 5",
    n >= 6 & n <= 10 ~ "6 to 10",
    n >= 11 & n <= 20 ~ "11 to 20",
    n >= 21 & n <= 50 ~ "21 to 50",
    n >= 51 & n <= 100 ~ "51 to 100",
    n >= 101 & n <= 500 ~ "100 to 500", # leurs intervalles ne sont mêmes pas convenables
  )) %>%
  mutate(FEATURE_ASSIGNMENT_INVESTIGATOR_NAME.E41 = "x") %>%
  select(-NAME.E41, -n) %>%
  relocate(FEATURE_ARRANGEMENT_TYPE.E55, .after = FEATURE_SHAPE_TYPE.E55)


# InterprGroup : feuille demandée par la RCU
sortie_InterprGroup <- data_pivot %>%
  filter(rcu_onglet == "InterprGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(INTERPRETATION_NUMBER_TYPE.E55 = "1") %>%
  mutate(FUNCTION_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41 = "x") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "INTERPRETATION_TYPE.I4") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "INTERPRETATION_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "INTERPRETATION_NUMBER_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "FUNCTION_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "FUNCTION_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41") %>%
  relocate(INTERPRETATION_NUMBER_TYPE.E55, .after = INTERPRETATION_CERTAINTY.I6)


# zDisturbanceGroup : feuille demandée par la RCU
sortie_zDisturbanceGroup <- data_pivot %>%
  filter(rcu_onglet == "zDisturbanceGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(DISTURBANCE_CAUSE_CERTAINTY.I6 = "High",
         EFFECT_CERTAINTY.I6 = "High",
         DISTURBANCE_DATE_FROM.E61 = "x",
         DISTURBANCE_DATE_TO.E61 = "x",
         DISTURBANCE_DATE_OCCURRED_ON.E61 = "x",
         DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41 = "x") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "DISTURBANCE_CAUSE_TYPE.I4", 
                             variable_revue = "DISTURBANCE_DATE_OCCURRED_BEFORE.E61") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "DISTURBANCE_CAUSE_TYPE.I4", 
                             variable_revue = "DISTURBANCE_CAUSE_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "DISTURBANCE_CAUSE_TYPE.I4", 
                             variable_revue = "EFFECT_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "DISTURBANCE_CAUSE_TYPE.I4", 
                             variable_revue = "DISTURBANCE_DATE_FROM.E61") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "DISTURBANCE_CAUSE_TYPE.I4", 
                             variable_revue = "DISTURBANCE_DATE_TO.E61") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "DISTURBANCE_CAUSE_TYPE.I4", 
                             variable_revue = "DISTURBANCE_DATE_OCCURRED_ON.E61") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "DISTURBANCE_CAUSE_TYPE.I4", 
                             variable_revue = "DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41") %>%
  relocate(DISTURBANCE_CAUSE_TYPE.I4, .after = DISTURBANCE_CAUSE_CATEGORY_TYPE.E55) %>%
  relocate(DISTURBANCE_CAUSE_CERTAINTY.I6, .after = DISTURBANCE_CAUSE_TYPE.I4) %>%
  relocate(EFFECT_CERTAINTY.I6, .after = EFFECT_TYPE.I4) %>%
  relocate(DISTURBANCE_DATE_OCCURRED_BEFORE.E61, .after = DISTURBANCE_DATE_TO.E61)


# ThreatGroup : je le mets dans une feuille > il faut demandé à Laura si c'est comme dans le cas des features
sorties_ThreatGroup <- data_pivot %>%
  filter(rcu_onglet == "ThreatGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(THREAT_INFERENCE_MAKING_ASSESSOR_NAME.E41 = "UCOP Team") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "THREAT_TYPE.I4", 
                             variable_revue = "THREAT_INFERENCE_MAKING_ASSESSOR_NAME.E41") %>%
  relocate(THREAT_CATEGORY.I4, .before = THREAT_TYPE.I4)


# NOT : feuille demandée par la RCU
sortie_NOT <- data_pivot %>%
  filter(rcu_onglet == "NOT") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(HERITAGE_PLACE_TYPE.E55 = "Heritage Place (Individual)",
         FEATURE_MORPHOLOGY_TYPE.E55 = "Positive/Built Feature",
         SITE_OVERALL_SHAPE_TYPE.E55 = "Polygonal",
         GEOMETRY_EXTENT_CERTAINTY.I6 = "High",
         OVERALL_ARCHAEOLOGICAL_CERTAINTY_VALUE.I6 = "Definite",
         TOPOGRAPHY_TYPE.E55 = "Valley Bed") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "GRID_ID.E42", 
                             variable_revue = "HERITAGE_PLACE_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "GRID_ID.E42", 
                             variable_revue = "FEATURE_MORPHOLOGY_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "GRID_ID.E42", 
                             variable_revue = "SITE_OVERALL_SHAPE_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "GRID_ID.E42", 
                             variable_revue = "GEOMETRY_EXTENT_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "GRID_ID.E42", 
                             variable_revue = "OVERALL_ARCHAEOLOGICAL_CERTAINTY_VALUE.I6") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "GRID_ID.E42", 
                             variable_revue = "TOPOGRAPHY_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "GRID_ID.E42", 
                             variable_revue = "OVERALL_CONDITION_STATE_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "GRID_ID.E42", 
                             variable_revue = "DAMAGE_EXTENT_TYPE.E55") %>%
  relocate(GRID_ID.E42, .after = OVERALL_ARCHAEOLOGICAL_CERTAINTY_VALUE.I6) %>%
  relocate(OVERALL_CONDITION_STATE_TYPE.E55, .after = TOPOGRAPHY_TYPE.E55) %>%
  relocate(DAMAGE_EXTENT_TYPE.E55, .after = OVERALL_CONDITION_STATE_TYPE.E55)


# GeometryGroup : feuille demandée par la RCU
sortie_GeometryGroup <- heritage_place_gis %>%
  st_transform(x = ., crs = 4326) %>%
  select(geom) %>%
  mutate(GEOMETRIC_PLACE_EXPRESSION.SP5 = lwgeom::st_astext(geom)) %>%
  st_drop_geometry() %>%
  mutate(LOCATION_CERTAINTY.I6 = "Definite") %>%
  relocate(LOCATION_CERTAINTY.I6, .before = GEOMETRIC_PLACE_EXPRESSION.SP5)

# verification
tm_shape(st_as_sf(st_as_sfc(sortie_GeometryGroup$GEOMETRIC_PLACE_EXPRESSION.SP5))) + tm_polygons()


# MeasurementGroup : feuille demandée par la RCU
# construction à partir d'opération géométrique sur les heritage places
# see http://github.com/Archaios-archeo/ucop_bulk_rcu/blob/main/spatial_operations_on_features_and_places.R
sortie_MeasurementGroup <- heritage_place_tibble %>%
  select(length_ok, width_ok) %>%
  mutate(MEASUREMENT_NUMBER.E60 = str_c(length_ok, width_ok, sep = "|"),
         MEASUREMENT_UNIT.E58 = "meters|meters",
         DIMENSION_TYPE.E55 = "length|breadth/width",
         MEASUREMENT_SOURCE_TYPE.E55 = "Estimated|Estimated") %>%
  select(-length_ok, -width_ok)
  
  
#### sorties par sheets dans un même fichier excel ####
list_of_datasets <- list("zzAssessment" = sortie_zzAssessment, 
                         "NameGroup" = sortie_NameGroup,
                         "GeometryGroup" = sortie_GeometryGroup,
                         "MeasurementGroup" = sortie_MeasurementGroup,
                         "DescriptionGroup" = sortie_DescriptionGroup,
                         "AdminAreasGroup" = sortie_AdminAreasGroup,
                         "PeriodGroup" = sortie_PeriodGroup,
                         "FeatureGroup" = sortie_FeatureGroup,
                         "InterprGroup" = sortie_InterprGroup,
                         "ThreatGroup"  = sorties_ThreatGroup,
                         "zDisturbanceGroup" = sortie_zDisturbanceGroup,
                         "NOT" = sortie_NOT)
write.xlsx(list_of_datasets, file = "sorties/finales/2019/UCOP_heritage_places_2019.xlsx", append = TRUE)


