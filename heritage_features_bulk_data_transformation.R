library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(openxlsx) # pour la sortie du fichier excel
tmap_mode("view")

# sources fonctions
source("bulk_functions.R")

#### DATA SOURCES ####
# general database
ucop_data_2019_2020_1 <- read_excel(path = "data/ucop_data_2019_2020_1_v5.xlsx")

ucop_data_500 <- ucop_data_2019_2020_1 %>%
  slice(3501:4000) %>%
  arrange(OS_Number)

# spatial data-compilation from heritage places spatial operations
# see http://github.com/Archaios-archeo/ucop_bulk_rcu/blob/main/spatial_operations_on_features_and_places.R
heritage_features_polygons_gis <- st_read("sorties/finales/500_features_bulk_8/donnees_spatiales_features_jg.gpkg", layer = "polygons")

heritage_features_lines_gis <- st_read("sorties/finales/500_features_bulk_8/donnees_spatiales_features_jg.gpkg", layer = "lines")

heritage_features_points_gis <- st_read("sorties/finales/500_features_bulk_7/donnees_spatiales_features_jg.gpkg", layer = "points")


# il y a les "alternatives references", à savoir les V_ (via blabla) et P_ (pur plots blabla) qu'il faut reprendre et intégrer
# dans les pour des questions de traçabilité de nos données
correspondances_via_plot_os <- read_excel(path = "data/correspondances_os_plot_via.xlsx")

#### POLYGONS PROCESS ####
heritage_features_polygons_tibble <- heritage_features_polygons_gis %>%
  st_drop_geometry() %>%
  left_join(., y = ucop_data_2019_2020_1, by = c("FEATURE_ID" = "OS_Number")) %>%
  as_tibble() %>%
  select(-n)


#### création du fichier pour le "bulk apload" ####
# relation table between UCOP data colmun and IDIHA column :
lien_descripteurs <- read.csv("data/relations_rcu_idiha_ucop_features.csv", header = TRUE, stringsAsFactors = FALSE,
                              encoding = "UTF-8") %>%
  as_tibble()


## Réorganisation des données en entrée pour correspondre au différentes feuilles demandées par la RCU dans IDIHA
data_pivot <- heritage_features_polygons_tibble %>%
  select(-numero, -date_modif, -voie) %>%
  rowid_to_column("ID") %>% # pour faire un pivot_longer complet (avec toutes les colonnes initiales) ensuite
  pivot_longer(data = ., cols = -ID, 
               names_to = "ucop_real_name", 
               values_to = "valeurs_ucop_origine") %>% # pivot pour faire ensuite la jointure avec les noms de IDIHA
  left_join(., y = lien_descripteurs %>% # jointure avec le tableau de correspondance des noms IDIHA
              select(rcu_entite:if_yes_what), # sélection des éléments possiblement utiles, le reste étant perso, 
            by = "ucop_real_name") %>%
  filter(rcu_entite == "_Features")


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
  mutate(INVESTIGATOR_ROLE_TYPE.E55 = "UCOP Project") %>%
  # création de la colonne investigator qui est lié à un "pattern" systématique > (ajout LM en septembre 2021)
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
         # ajout systématique obligatoire de mon propre nom en tant que travail de desk-based, à la date du code, et en tant que membre IDIHA
         INVESTIGATOR_ROLE_TYPE.E55 = str_c(INVESTIGATOR_ROLE_TYPE.E55, "UCOP Project", sep = "|"),
         ASSESSMENT_ACTIVITY_TYPE.E55 = str_c(ASSESSMENT_ACTIVITY_TYPE.E55, "Desk-based (Unspecified)", sep = "|"),
         ASSESSMENT_ACTIVITY_DATE.E49 = str_c(ASSESSMENT_ACTIVITY_DATE.E49, Sys.Date(), sep = "|"))


# NameGroup : feuille demandée par la RCU
sortie_NameGroup <- data_pivot %>%
  filter(rcu_onglet == "NameGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(NAME_TYPE.E55 = "Alternative Reference") %>%
  left_join(., y = correspondances_via_plot_os, by = c("NAME.E41" = "OS_Number")) %>%
  mutate(NAME.E41 = case_when(
    !is.na(Old_Number) ~ paste0(NAME.E41, "|", Old_Number),
    TRUE ~ NAME.E41
  )) %>%
  select(-Old_Number) %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "NAME.E41", 
                             variable_revue = "NAME_TYPE.E55")


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
  )) %>%
  bind_cols(sortie_NameGroup) %>%
  left_join(., y = ucop_data_500 %>%
              select(OS_Number, `Feature significance RCU`), 
            by = c("NAME.E41" = "OS_Number")) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`)) %>%
  select(GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62)


# FeatureFormGroup : feuille demandée par la RCU
sortie_FeatureFormGroup <- data_pivot %>%
  filter(rcu_onglet == "FeatureFormGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(FORM_NUMBER.E55 = "1",
         FORM_ASSIGNMENT_INVESTIGATOR_NAME.E41 = "UCOP Team") %>%
  relocate(FORM_ARRANGEMENT.E55, .after = FORM_SHAPE_TYPE.E55)


# InterpretationGroup : feuille demandée par la RCU
# cette liste dans la RCU est particulière pour nous car elle demande :
# 1) de faire de la répétition de pattern sur les périodes
# 2) puis sur les feature functions
# car les deux "main" variables-lists sont composées d'éléments multiples
sortie_InterpretationGroup <- data_pivot %>%
  filter(rcu_onglet == "InterpretationGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(CULTURAL_PERIOD_CERTAINTY.I6 = case_when(
    CULTURAL_PERIOD_CERTAINTY.I6 == "Low" ~ "Possible",
    CULTURAL_PERIOD_CERTAINTY.I6 == "Medium" ~ "Probable",
    CULTURAL_PERIOD_CERTAINTY.I6 == "High" ~ "Definite",
    TRUE ~  CULTURAL_PERIOD_CERTAINTY.I6
  )) %>%
  mutate(ARCHAEOLOGICAL_FROM_DATE.E61 = "x",
         ARCHAEOLOGICAL_TO_DATE.E61 = "x",
         DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41 = "UCOP Team") %>%
  #  répétition par rapport aux périodes chronologiques
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "CULTURAL_PERIOD_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "ARCHAEOLOGICAL_FROM_DATE.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "ARCHAEOLOGICAL_TO_DATE.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "FUNCTION_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "INTERPRETATION_TYPE.I4") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "INTERPRETATION_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "FUNCTION_TYPE.I4") %>%
  # répétition par rapport aux fonctions des features
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "CULTURAL_PERIOD_TYPE.I4") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "CULTURAL_PERIOD_DETAIL_TYPE.E55") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "CULTURAL_PERIOD_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "ARCHAEOLOGICAL_FROM_DATE.E61") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "ARCHAEOLOGICAL_TO_DATE.E61") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "FUNCTION_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "INTERPRETATION_TYPE.I4") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "INTERPRETATION_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                             variable_revue = "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41") %>%
  # transformation glossaire
  mutate(FUNCTION_TYPE.I4 = str_replace_all(string = FUNCTION_TYPE.I4, pattern = "Public", replacement = "Public/Institutional")) %>%
  relocate(any_of(
    c("CULTURAL_PERIOD_TYPE.I4", "CULTURAL_PERIOD_DETAIL_TYPE.E55", "CULTURAL_PERIOD_CERTAINTY.I6",
      "ARCHAEOLOGICAL_FROM_DATE.E61", "ARCHAEOLOGICAL_TO_DATE.E61", "FUNCTION_TYPE.I4",
      "FUNCTION_CERTAINTY.I6", "INTERPRETATION_TYPE.I4", "INTERPRETATION_CERTAINTY.I6",
      "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41")
  )) %>%
  # ajout d'une transformation pour que les patterns temporels soient cohérents avec les patterns fonctionnels
  mutate(CULTURAL_PERIOD_TYPE.I4 = if_else(
    condition = CULTURAL_PERIOD_TYPE.I4 == "Modern|Islamic|Modern|Islamic",
    true = "Modern|Modern|Islamic|Islamic",
    false = CULTURAL_PERIOD_TYPE.I4)) %>%
  mutate(CULTURAL_PERIOD_DETAIL_TYPE.E55 = if_else(CULTURAL_PERIOD_DETAIL_TYPE.E55 == "Unknown|Late Ottoman|Unknown|Late Ottoman",
    true = "Unknown|Unknown|Late Ottoman|Late Ottoman",
    false = CULTURAL_PERIOD_DETAIL_TYPE.E55))


# AdminAreasGroup : feuille demandée par la RCU
sortie_AdminAreasGroup <- tibble(ADMINISTRATIVE_DIVISION_NAME.E44 = "Al Ula",
                                 ADMINISTRATIVE_DIVISION_TYPE.E55 = "County/Governorate") %>%
  bind_rows(
    replicate(n = nrow(sortie_zzAssessment) - 1, expr = ., simplify = FALSE)
  )


# NOT : feuille demandée par la RCU
sortie_NOT <- data_pivot %>%
  filter(rcu_onglet == "NOT") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(HERITAGE_CLASSIFICATION_TYPE.E55 = "Individual Feature",
         GEOMETRY_EXTENT_CERTAINTY.I6 = "Definite",
           TOPOGRAPHY_TYPE.E55 = "Valley Bed") %>%
  relocate(any_of(
    c("HERITAGE_CLASSIFICATION_TYPE.E55", "HERITAGE_FEATURE_USE_TYPE.E55", "GEOMETRY_EXTENT_CERTAINTY.I6",
      "DAMAGE_EXTENT_TYPE.E55", "OVERALL_CONDITION_STATE_TYPE.E55", "TOPOGRAPHY_TYPE.E55")
  )) %>%
  mutate(OVERALL_CONDITION_STATE_TYPE.E55 = case_when(
    OVERALL_CONDITION_STATE_TYPE.E55 == "Good" ~ "Intact/No Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Excellent" ~ "Intact/No Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Fair" ~ "Slight Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Poor" ~ "Moderate Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Very Bad" ~ "Severe Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Unknown" ~ "No Visible/Accessible/Known",
    OVERALL_CONDITION_STATE_TYPE.E55 %in% c("n.a.", "na", "n.a", "na.", "NA") ~ "x",
    TRUE ~ OVERALL_CONDITION_STATE_TYPE.E55
  )) %>%
  rename(OVERALL_DAMAGE_SEVERITY_TYPE.E55 = OVERALL_CONDITION_STATE_TYPE.E55) %>%
  mutate(GRID_ID.E42 = if_else(is.na(GRID_ID.E42), "x", GRID_ID.E42))


# zDisturbanceGroup : feuille demandée par la RCU
sortie_zDisturbanceGroup <- data_pivot %>%
  mutate(valeurs_ucop_origine = if_else(
    condition = valeurs_ucop_origine == "Erosion / Deterioration",
    true = "Erosion/Deterioration",
    false = valeurs_ucop_origine
  )) %>%
  filter(rcu_onglet == "zDisturbanceGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(DAMAGE_TREND_TYPE.E55 = "x",
         DISTURBANCE_CAUSE_CERTAINTY.I6 = "Definite",
         DISTURBANCE_DATE_FROM.E61 = "x",
         DISTURBANCE_DATE_TO.E61 = "x",
         DISTURBANCE_DATE_OCCURRED_ON.E61 = "x",
         DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41 = "UCOP Team") %>%
  mutate(DISTURBANCE_CAUSE_CATEGORY_TYPE.E55 = if_else(
    condition = is.na(DISTURBANCE_CAUSE_CATEGORY_TYPE.E55),
    true = "Unknown",
    DISTURBANCE_CAUSE_CATEGORY_TYPE.E55
  )) %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DAMAGE_TREND_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_CAUSE_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_FROM.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_TO.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_OCCURRED_ON.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_OCCURRED_BEFORE.E61") %>%
  relocate(any_of(
    c("DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", "EFFECT_TYPE.S9", "DAMAGE_TREND_TYPE.E55",
      "DISTURBANCE_CAUSE_TYPE.I4", "DISTURBANCE_CAUSE_CERTAINTY.I6", "DISTURBANCE_DATE_FROM.E61",
      "DISTURBANCE_DATE_TO.E61", "DISTURBANCE_DATE_OCCURRED_BEFORE.E61", "DISTURBANCE_DATE_OCCURRED_ON.E61",
      "DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41")
  ))


# ThreatGroup : feuille demandée par la RCU
sortie_ThreatGroup <- data_pivot %>%
  filter(rcu_onglet == "ThreatGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(POTENTIAL_IMPACT_TYPE.E55 = "x",
         SECONDARY_THREAT_TYPE.E55 = "x",
         EXPOSURE.E62 = "x",
         RISK_EXTENT_TYPE.E55 = "x",
         RISK_SEVERITY_TYPE.E55 = "x",
         RISK_LEVEL.I4 = "x",
         RISK_LEVEL_CERTAINTY.I6 = "x",
         RISK_EVALUATION_ASSESSOR_NAME.E41 = "UCOP Team") %>%
  mutate(THREAT_TYPE.E55 = if_else(
    condition = is.na(THREAT_TYPE.E55),
    true = "Unknown",
    THREAT_TYPE.E55
  )) %>% # si NA dans la base de données > assignation en "Unknown" pour être en mesure d'appliquer les fonctions suivantes
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "POTENTIAL_IMPACT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "SECONDARY_THREAT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "EXPOSURE.E62") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_EXTENT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_SEVERITY_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_LEVEL.I4") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_LEVEL_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_EVALUATION_ASSESSOR_NAME.E41") %>%
  relocate(VULNERABILITY_TYPE.E55, .after = EXPOSURE.E62) %>%
  relocate(THREAT_PROBABILITY_TYPE.E55, .after = VULNERABILITY_TYPE.E55) %>%
  mutate(THREAT_PROBABILITY_TYPE.E55 = str_replace_all(string = THREAT_PROBABILITY_TYPE.E55, 
                                                       pattern = c("Possible" = "2 - Possible",
                                                                   "Probable" = "4 - Probable",
                                                                   "Unlikely" = "1 - Unlikely",
                                                                   "Likely" = "3 - Likely",
                                                                   "Certain" = "5 - Certain"))) %>%
  select(-VULNERABILITY_TYPE.E55) # en fait, n'existe pas pour les heritage features


# GeometryGroup : feuille demandée par la RCU
sortie_GeometryGroup <- heritage_features_polygons_gis %>%
  st_transform(x = ., crs = 4326) %>%
  select(geom) %>%
  mutate(GEOMETRIC_PLACE_EXPRESSION.SP5 = lwgeom::st_astext(geom)) %>%
  st_drop_geometry() %>%
  mutate(LOCATION_CERTAINTY.I6 = "Definite") %>%
  relocate(LOCATION_CERTAINTY.I6, .before = GEOMETRIC_PLACE_EXPRESSION.SP5)

# verification
tm_shape(st_as_sf(st_as_sfc(sortie_GeometryGroup$GEOMETRIC_PLACE_EXPRESSION.SP5))) + tm_polygons() + tmap_options(check.and.fix = TRUE)


# MeasurementGroup : feuille demandée par la RCU
sortie_MeasurementGroup <- heritage_features_polygons_tibble %>%
  select(Length, Width, Height) %>%
  mutate(Length = if_else(is.na(Length), "x", Length),
         Width = if_else(is.na(Width), "x", Width),
         Height = if_else(is.na(Height), "x", Height)) %>%
  mutate(MEASUREMENT_NUMBER.E60 = str_c(Length, Width, Height, sep = "|")) %>%
  mutate(MEASUREMENT_UNIT.E58 = "meters|meters|meters",
         DIMENSION_TYPE.E55 = "length|breadth/width|height",
         MEASUREMENT_SOURCE_TYPE.E55 = "Estimated|Estimated|Estimated") %>%
  select(-Length:-Height)



#### sorties par sheets dans un même fichier excel ####
list_of_datasets <- list("zzAssessment" = sortie_zzAssessment, 
                         "NameGroup" = sortie_NameGroup,
                         "GeometryGroup" = sortie_GeometryGroup,
                         "MeasurementGroup" = sortie_MeasurementGroup,
                         "DescriptionGroup" = sortie_DescriptionGroup,
                         "FeatureFormGroup" = sortie_FeatureFormGroup,
                         "InterpretationGroup" = sortie_InterpretationGroup,
                         "AdminAreasGroup" = sortie_AdminAreasGroup,
                         "NOT" = sortie_NOT,
                         "zDisturbanceGroup" = sortie_zDisturbanceGroup,
                         "ThreatGroup" = sortie_ThreatGroup)

openxlsx::write.xlsx(list_of_datasets, file = "sorties/finales/500_features_bulk_8/UCOP_heritage_features_bulk.xlsx")

rm(heritage_features_polygons_gis, heritage_features_polygons_tibble, sortie_NameGroup, sortie_AdminAreasGroup,
   sortie_DescriptionGroup, sortie_FeatureFormGroup, sortie_GeometryGroup, sortie_InterpretationGroup,
   sortie_MeasurementGroup, sortie_NOT, sortie_ThreatGroup, sortie_zDisturbanceGroup, sortie_zzAssessment)


#### LINES PROCESS ####
heritage_features_lines_tibble <- heritage_features_lines_gis %>%
  st_drop_geometry() %>%
  left_join(., y = ucop_data_2019_2020_1, by = c("FEATURE_ID" = "OS_Number")) %>%
  as_tibble()


## Réorganisation des données en entrée pour correspondre au différentes feuilles demandées par la RCU dans IDIHA
data_pivot <- heritage_features_lines_tibble %>%
  select(-numero, -date_modif, -voie) %>%
  rowid_to_column("ID") %>% # pour faire un pivot_longer complet (avec toutes les colonnes initiales) ensuite
  pivot_longer(data = ., cols = -ID, 
               names_to = "ucop_real_name", 
               values_to = "valeurs_ucop_origine") %>% # pivot pour faire ensuite la jointure avec les noms de IDIHA
  left_join(., y = lien_descripteurs %>% # jointure avec le tableau de correspondance des noms IDIHA
              select(rcu_entite:if_yes_what), # sélection des éléments possiblement utiles, le reste étant perso, 
            by = "ucop_real_name") %>%
  filter(rcu_entite == "_Features")


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
  mutate(INVESTIGATOR_ROLE_TYPE.E55 = "UCOP Project") %>%
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
         # ajout systématique obligatoire de mon propre nom en tant que travail de desk-based, à la date du code, et en tant que membre IDIHA
         INVESTIGATOR_ROLE_TYPE.E55 = str_c(INVESTIGATOR_ROLE_TYPE.E55, "UCOP Project", sep = "|"),
         ASSESSMENT_ACTIVITY_TYPE.E55 = str_c(ASSESSMENT_ACTIVITY_TYPE.E55, "Desk-based (Unspecified)", sep = "|"),
         ASSESSMENT_ACTIVITY_DATE.E49 = str_c(ASSESSMENT_ACTIVITY_DATE.E49, Sys.Date(), sep = "|"))


# NameGroup : feuille demandée par la RCU
sortie_NameGroup <- data_pivot %>%
  filter(rcu_onglet == "NameGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(NAME_TYPE.E55 = "Alternative Reference") %>%
  left_join(., y = correspondances_via_plot_os, by = c("NAME.E41" = "OS_Number")) %>%
  mutate(NAME.E41 = case_when(
    !is.na(Old_Number) ~ paste0(NAME.E41, "|", Old_Number),
    TRUE ~ NAME.E41
  )) %>%
  select(-Old_Number) %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "NAME.E41", 
                             variable_revue = "NAME_TYPE.E55")


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
  )) %>%
  bind_cols(sortie_NameGroup) %>%
  left_join(., y = ucop_data_500 %>%
              select(OS_Number, `Feature significance RCU`), 
            by = c("NAME.E41" = "OS_Number")) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`)) %>%
  select(GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62)


# FeatureFormGroup : feuille demandée par la RCU
sortie_FeatureFormGroup <- data_pivot %>%
  filter(rcu_onglet == "FeatureFormGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(FORM_NUMBER.E55 = "1",
         FORM_ASSIGNMENT_INVESTIGATOR_NAME.E41 = "UCOP Team") %>%
  relocate(FORM_ARRANGEMENT.E55, .after = FORM_SHAPE_TYPE.E55)


# InterpretationGroup : feuille demandée par la RCU
# cette liste dans la RCU est particulière pour nous car elle demande :
# 1) de faire de la répétition de pattern sur les périodes
# 2) puis sur les feature functions
# car les deux "main" variables-lists sont composées d'éléments multiples
sortie_InterpretationGroup <- data_pivot %>%
  filter(rcu_onglet == "InterpretationGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(CULTURAL_PERIOD_CERTAINTY.I6 = case_when(
    CULTURAL_PERIOD_CERTAINTY.I6 == "Low" ~ "Possible",
    CULTURAL_PERIOD_CERTAINTY.I6 == "Medium" ~ "Probable",
    CULTURAL_PERIOD_CERTAINTY.I6 == "High" ~ "Definite",
    TRUE ~  CULTURAL_PERIOD_CERTAINTY.I6
  )) %>%
  mutate(ARCHAEOLOGICAL_FROM_DATE.E61 = "x",
         ARCHAEOLOGICAL_TO_DATE.E61 = "x",
         DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41 = "UCOP Team") %>%
  #  répétition par rapport aux périodes chronologiques
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "CULTURAL_PERIOD_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "ARCHAEOLOGICAL_FROM_DATE.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "ARCHAEOLOGICAL_TO_DATE.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "FUNCTION_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "INTERPRETATION_TYPE.I4") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "INTERPRETATION_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "FUNCTION_TYPE.I4") %>%
  # répétition par rapport aux fonctions des features
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "CULTURAL_PERIOD_TYPE.I4") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "CULTURAL_PERIOD_DETAIL_TYPE.E55") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "CULTURAL_PERIOD_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "ARCHAEOLOGICAL_FROM_DATE.E61") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "ARCHAEOLOGICAL_TO_DATE.E61") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "FUNCTION_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "INTERPRETATION_TYPE.I4") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "INTERPRETATION_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41") %>%
  # transformation glossaire
  mutate(FUNCTION_TYPE.I4 = str_replace_all(string = FUNCTION_TYPE.I4, pattern = "Public", replacement = "Public/Institutional")) %>%
  relocate(any_of(
    c("CULTURAL_PERIOD_TYPE.I4", "CULTURAL_PERIOD_DETAIL_TYPE.E55", "CULTURAL_PERIOD_CERTAINTY.I6",
      "ARCHAEOLOGICAL_FROM_DATE.E61", "ARCHAEOLOGICAL_TO_DATE.E61", "FUNCTION_TYPE.I4",
      "FUNCTION_CERTAINTY.I6", "INTERPRETATION_TYPE.I4", "INTERPRETATION_CERTAINTY.I6",
      "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41")
  )) %>%
  mutate(CULTURAL_PERIOD_TYPE.I4 = if_else(
    condition = CULTURAL_PERIOD_TYPE.I4 == "Modern|Islamic|Modern|Islamic",
    true = "Modern|Modern|Islamic|Islamic",
    false = CULTURAL_PERIOD_TYPE.I4)) %>%
  mutate(CULTURAL_PERIOD_DETAIL_TYPE.E55 = if_else(CULTURAL_PERIOD_DETAIL_TYPE.E55 == "Unknown|Late Ottoman|Unknown|Late Ottoman",
                                                   true = "Unknown|Unknown|Late Ottoman|Late Ottoman",
                                                   false = CULTURAL_PERIOD_DETAIL_TYPE.E55))


# AdminAreasGroup : feuille demandée par la RCU
sortie_AdminAreasGroup <- tibble(ADMINISTRATIVE_DIVISION_NAME.E44 = "Al Ula",
                                 ADMINISTRATIVE_DIVISION_TYPE.E55 = "County/Governorate") %>%
  bind_rows(
    replicate(n = nrow(sortie_zzAssessment) - 1, expr = ., simplify = FALSE)
  )


# NOT : feuille demandée par la RCU
sortie_NOT <- data_pivot %>%
  filter(rcu_onglet == "NOT") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(HERITAGE_CLASSIFICATION_TYPE.E55 = "Individual Feature",
         GEOMETRY_EXTENT_CERTAINTY.I6 = "Definite",
         TOPOGRAPHY_TYPE.E55 = "Valley Bed") %>%
  relocate(any_of(
    c("HERITAGE_CLASSIFICATION_TYPE.E55", "HERITAGE_FEATURE_USE_TYPE.E55", "GEOMETRY_EXTENT_CERTAINTY.I6",
      "DAMAGE_EXTENT_TYPE.E55", "OVERALL_CONDITION_STATE_TYPE.E55", "TOPOGRAPHY_TYPE.E55")
  )) %>%
  mutate(OVERALL_CONDITION_STATE_TYPE.E55 = case_when(
    OVERALL_CONDITION_STATE_TYPE.E55 == "Good" ~ "Intact/No Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Excellent" ~ "Intact/No Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Fair" ~ "Slight Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Poor" ~ "Moderate Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Very Bad" ~ "Severe Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Unknown" ~ "No Visible/Accessible/Known",
    OVERALL_CONDITION_STATE_TYPE.E55 %in% c("n.a.", "na", "n.a", "na.", "NA") ~ "x",
    TRUE ~ OVERALL_CONDITION_STATE_TYPE.E55
  )) %>%
  rename(OVERALL_DAMAGE_SEVERITY_TYPE.E55 = OVERALL_CONDITION_STATE_TYPE.E55)



# zDisturbanceGroup : feuille demandée par la RCU
sortie_zDisturbanceGroup <- data_pivot %>%
  mutate(valeurs_ucop_origine = if_else(
    condition = valeurs_ucop_origine == "Erosion / Deterioration",
    true = "Erosion/Deterioration",
    false = valeurs_ucop_origine
  )) %>%
  filter(rcu_onglet == "zDisturbanceGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(DAMAGE_TREND_TYPE.E55 = "x",
         DISTURBANCE_CAUSE_CERTAINTY.I6 = "Definite",
         DISTURBANCE_DATE_FROM.E61 = "x",
         DISTURBANCE_DATE_TO.E61 = "x",
         DISTURBANCE_DATE_OCCURRED_ON.E61 = "x",
         DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41 = "UCOP Team") %>%
  mutate(DISTURBANCE_CAUSE_CATEGORY_TYPE.E55 = if_else(
    condition = is.na(DISTURBANCE_CAUSE_CATEGORY_TYPE.E55),
    true = "Unknown",
    DISTURBANCE_CAUSE_CATEGORY_TYPE.E55
  )) %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DAMAGE_TREND_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_CAUSE_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_FROM.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_TO.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_OCCURRED_ON.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_OCCURRED_BEFORE.E61") %>%
  relocate(any_of(
    c("DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", "EFFECT_TYPE.S9", "DAMAGE_TREND_TYPE.E55",
      "DISTURBANCE_CAUSE_TYPE.I4", "DISTURBANCE_CAUSE_CERTAINTY.I6", "DISTURBANCE_DATE_FROM.E61",
      "DISTURBANCE_DATE_TO.E61", "DISTURBANCE_DATE_OCCURRED_BEFORE.E61", "DISTURBANCE_DATE_OCCURRED_ON.E61",
      "DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41")
  ))


# ThreatGroup : feuille demandée par la RCU
sortie_ThreatGroup <- data_pivot %>%
  filter(rcu_onglet == "ThreatGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(POTENTIAL_IMPACT_TYPE.E55 = "x",
         SECONDARY_THREAT_TYPE.E55 = "x",
         EXPOSURE.E62 = "x",
         RISK_EXTENT_TYPE.E55 = "x",
         RISK_SEVERITY_TYPE.E55 = "x",
         RISK_LEVEL.I4 = "x",
         RISK_LEVEL_CERTAINTY.I6 = "x",
         RISK_EVALUATION_ASSESSOR_NAME.E41 = "UCOP Team") %>%
  mutate(THREAT_TYPE.E55 = if_else(
    condition = is.na(THREAT_TYPE.E55),
    true = "Unknown",
    THREAT_TYPE.E55
  )) %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "POTENTIAL_IMPACT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "SECONDARY_THREAT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "EXPOSURE.E62") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_EXTENT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_SEVERITY_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_LEVEL.I4") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_LEVEL_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_EVALUATION_ASSESSOR_NAME.E41") %>%
  relocate(VULNERABILITY_TYPE.E55, .after = EXPOSURE.E62) %>%
  relocate(THREAT_PROBABILITY_TYPE.E55, .after = VULNERABILITY_TYPE.E55)  %>%
  mutate(THREAT_PROBABILITY_TYPE.E55 = str_replace_all(string = THREAT_PROBABILITY_TYPE.E55, 
                                                       pattern = c("Possible" = "2 - Possible",
                                                                   "Probable" = "4 - Probable",
                                                                   "Unlikely" = "1 - Unlikely",
                                                                   "Likely" = "3 - Likely",
                                                                   "Certain" = "5 - Certain"))) %>%
  select(-VULNERABILITY_TYPE.E55)


# GeometryGroup : feuille demandée par la RCU
sortie_GeometryGroup <- heritage_features_lines_gis %>%
  st_transform(x = ., crs = 4326) %>%
  select(geom) %>%
  mutate(GEOMETRIC_PLACE_EXPRESSION.SP5 = lwgeom::st_astext(geom)) %>%
  st_drop_geometry() %>%
  mutate(LOCATION_CERTAINTY.I6 = "Definite") %>%
  relocate(LOCATION_CERTAINTY.I6, .before = GEOMETRIC_PLACE_EXPRESSION.SP5)

# verification
tm_shape(st_as_sf(st_as_sfc(sortie_GeometryGroup$GEOMETRIC_PLACE_EXPRESSION.SP5))) + tm_lines()


# MeasurementGroup : feuille demandée par la RCU
sortie_MeasurementGroup <- heritage_features_lines_tibble %>%
  select(Length, Width, Height) %>%
  mutate(Length = if_else(is.na(Length), "x", Length),
         Width = if_else(is.na(Width), "x", Width),
         Height = if_else(is.na(Height), "x", Height)) %>%
  mutate(MEASUREMENT_NUMBER.E60 = str_c(Length, Width, Height, sep = "|")) %>%
  mutate(MEASUREMENT_UNIT.E58 = "meters|meters|meters",
         DIMENSION_TYPE.E55 = "length|breadth/width|height",
         MEASUREMENT_SOURCE_TYPE.E55 = "Estimated|Estimated|Estimated") %>%
  select(-Length:-Height)




#### sorties par sheets dans un même fichier excel ####
list_of_datasets <- list("zzAssessment" = sortie_zzAssessment, 
                         "NameGroup" = sortie_NameGroup,
                         "GeometryGroup" = sortie_GeometryGroup,
                         "MeasurementGroup" = sortie_MeasurementGroup,
                         "DescriptionGroup" = sortie_DescriptionGroup,
                         "FeatureFormGroup" = sortie_FeatureFormGroup,
                         "InterpretationGroup" = sortie_InterpretationGroup,
                         "AdminAreasGroup" = sortie_AdminAreasGroup,
                         "NOT" = sortie_NOT,
                         "zDisturbanceGroup" = sortie_zDisturbanceGroup,
                         "ThreatGroup" = sortie_ThreatGroup)

openxlsx::write.xlsx(list_of_datasets, file = "sorties/finales/500_features_bulk_8/UCOP_heritage_features_bulk_lines.xlsx")


rm(heritage_features_lines_gis, heritage_features_lines_tibble, sortie_NameGroup, sortie_AdminAreasGroup,
   sortie_DescriptionGroup, sortie_FeatureFormGroup, sortie_GeometryGroup, sortie_InterpretationGroup,
   sortie_MeasurementGroup, sortie_NOT, sortie_ThreatGroup, sortie_zDisturbanceGroup, sortie_zzAssessment)


#### POINTS PROCESS ####
heritage_features_points_tibble <- heritage_features_points_gis %>%
  st_drop_geometry() %>%
  left_join(., y = ucop_data_2019_2020_1, by = c("FEATURE_ID" = "OS_Number")) %>%
  as_tibble()


## Réorganisation des données en entrée pour correspondre au différentes feuilles demandées par la RCU dans IDIHA
data_pivot <- heritage_features_points_tibble %>%
  select(-numero, -date_modif, -voie) %>%
  rowid_to_column("ID") %>% # pour faire un pivot_longer complet (avec toutes les colonnes initiales) ensuite
  pivot_longer(data = ., cols = -ID, 
               names_to = "ucop_real_name", 
               values_to = "valeurs_ucop_origine") %>% # pivot pour faire ensuite la jointure avec les noms de IDIHA
  left_join(., y = lien_descripteurs %>% # jointure avec le tableau de correspondance des noms IDIHA
              select(rcu_entite:if_yes_what), # sélection des éléments possiblement utiles, le reste étant perso, 
            by = "ucop_real_name") %>%
  filter(rcu_entite == "_Features")


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
  mutate(INVESTIGATOR_ROLE_TYPE.E55 = "UCOP Project") %>%
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
         # ajout systématique obligatoire de mon propre nom en tant que travail de desk-based, à la date du code, et en tant que membre IDIHA
         INVESTIGATOR_ROLE_TYPE.E55 = str_c(INVESTIGATOR_ROLE_TYPE.E55, "UCOP Project", sep = "|"),
         ASSESSMENT_ACTIVITY_TYPE.E55 = str_c(ASSESSMENT_ACTIVITY_TYPE.E55, "Desk-based (Unspecified)", sep = "|"),
         ASSESSMENT_ACTIVITY_DATE.E49 = str_c(ASSESSMENT_ACTIVITY_DATE.E49, Sys.Date(), sep = "|"))


# NameGroup : feuille demandée par la RCU
sortie_NameGroup <- data_pivot %>%
  filter(rcu_onglet == "NameGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(NAME_TYPE.E55 = "Alternative Reference") %>%
  left_join(., y = correspondances_via_plot_os, by = c("NAME.E41" = "OS_Number")) %>%
  mutate(NAME.E41 = case_when(
    !is.na(Old_Number) ~ paste0(NAME.E41, "|", Old_Number),
    TRUE ~ NAME.E41
  )) %>%
  select(-Old_Number) %>%
  repetition_pattern_n_exact(x = ., 
                             variable_a_bon_pattern = "NAME.E41", 
                             variable_revue = "NAME_TYPE.E55")


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
  )) %>%
  bind_cols(sortie_NameGroup) %>%
  left_join(., y = ucop_data_500 %>%
              select(OS_Number, `Feature significance RCU`), 
            by = c("NAME.E41" = "OS_Number")) %>%
  mutate(GENERAL_DESCRIPTION_TYPE.E55 = paste0(GENERAL_DESCRIPTION_TYPE.E55, "|", "Summary of Significance")) %>%
  mutate(GENERAL_DESCRIPTION.E62 = paste0(GENERAL_DESCRIPTION.E62, "|", `Feature significance RCU`)) %>%
  select(GENERAL_DESCRIPTION_TYPE.E55, GENERAL_DESCRIPTION.E62)


# FeatureFormGroup : feuille demandée par la RCU
sortie_FeatureFormGroup <- data_pivot %>%
  filter(rcu_onglet == "FeatureFormGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(FORM_NUMBER.E55 = "1",
         FORM_ASSIGNMENT_INVESTIGATOR_NAME.E41 = "UCOP Team") %>%
  relocate(FORM_ARRANGEMENT.E55, .after = FORM_SHAPE_TYPE.E55)


# InterpretationGroup : feuille demandée par la RCU
# cette liste dans la RCU est particulière pour nous car elle demande :
# 1) de faire de la répétition de pattern sur les périodes
# 2) puis sur les feature functions
# car les deux "main" variables-lists sont composées d'éléments multiples
sortie_InterpretationGroup <- data_pivot %>%
  filter(rcu_onglet == "InterpretationGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(CULTURAL_PERIOD_CERTAINTY.I6 = case_when(
    CULTURAL_PERIOD_CERTAINTY.I6 == "Low" ~ "Possible",
    CULTURAL_PERIOD_CERTAINTY.I6 == "Medium" ~ "Probable",
    CULTURAL_PERIOD_CERTAINTY.I6 == "High" ~ "Definite",
    TRUE ~  CULTURAL_PERIOD_CERTAINTY.I6
  )) %>%
  mutate(ARCHAEOLOGICAL_FROM_DATE.E61 = "x",
         ARCHAEOLOGICAL_TO_DATE.E61 = "x",
         DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41 = "UCOP Team") %>%
  #  répétition par rapport aux périodes chronologiques
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "CULTURAL_PERIOD_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "ARCHAEOLOGICAL_FROM_DATE.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "ARCHAEOLOGICAL_TO_DATE.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "FUNCTION_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "INTERPRETATION_TYPE.I4") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "INTERPRETATION_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "CULTURAL_PERIOD_TYPE.I4", 
                             variable_revue = "FUNCTION_TYPE.I4") %>%
  # répétition par rapport aux fonctions des features
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "CULTURAL_PERIOD_TYPE.I4") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "CULTURAL_PERIOD_DETAIL_TYPE.E55") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "CULTURAL_PERIOD_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "ARCHAEOLOGICAL_FROM_DATE.E61") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "ARCHAEOLOGICAL_TO_DATE.E61") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "FUNCTION_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "INTERPRETATION_TYPE.I4") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "INTERPRETATION_CERTAINTY.I6") %>%
  repetition_pattern_n_if_sup_2(x = ., variable_a_bon_pattern = "FUNCTION_TYPE.I4", 
                                variable_revue = "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41") %>%
  # transformation glossaire
  mutate(FUNCTION_TYPE.I4 = str_replace_all(string = FUNCTION_TYPE.I4, pattern = "Public", replacement = "Public/Institutional")) %>%
  relocate(any_of(
    c("CULTURAL_PERIOD_TYPE.I4", "CULTURAL_PERIOD_DETAIL_TYPE.E55", "CULTURAL_PERIOD_CERTAINTY.I6",
      "ARCHAEOLOGICAL_FROM_DATE.E61", "ARCHAEOLOGICAL_TO_DATE.E61", "FUNCTION_TYPE.I4",
      "FUNCTION_CERTAINTY.I6", "INTERPRETATION_TYPE.I4", "INTERPRETATION_CERTAINTY.I6",
      "DATE_INTERPRETATION_INFERENCE_MAKING_ACTOR_NAME.E41")
  )) %>%
  mutate(CULTURAL_PERIOD_TYPE.I4 = if_else(
    condition = CULTURAL_PERIOD_TYPE.I4 == "Modern|Islamic|Modern|Islamic",
    true = "Modern|Modern|Islamic|Islamic",
    false = CULTURAL_PERIOD_TYPE.I4)) %>%
  mutate(CULTURAL_PERIOD_DETAIL_TYPE.E55 = if_else(CULTURAL_PERIOD_DETAIL_TYPE.E55 == "Unknown|Late Ottoman|Unknown|Late Ottoman",
                                                   true = "Unknown|Unknown|Late Ottoman|Late Ottoman",
                                                   false = CULTURAL_PERIOD_DETAIL_TYPE.E55))


# AdminAreasGroup : feuille demandée par la RCU
sortie_AdminAreasGroup <- tibble(ADMINISTRATIVE_DIVISION_NAME.E44 = "Al Ula",
                                 ADMINISTRATIVE_DIVISION_TYPE.E55 = "County/Governorate") %>%
  bind_rows(
    replicate(n = nrow(sortie_zzAssessment) - 1, expr = ., simplify = FALSE)
  )


# NOT : feuille demandée par la RCU
sortie_NOT <- data_pivot %>%
  filter(rcu_onglet == "NOT") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(HERITAGE_CLASSIFICATION_TYPE.E55 = "Individual Feature",
         GEOMETRY_EXTENT_CERTAINTY.I6 = "Definite",
         TOPOGRAPHY_TYPE.E55 = "Valley Bed") %>%
  relocate(any_of(
    c("HERITAGE_CLASSIFICATION_TYPE.E55", "HERITAGE_FEATURE_USE_TYPE.E55", "GEOMETRY_EXTENT_CERTAINTY.I6",
      "DAMAGE_EXTENT_TYPE.E55", "OVERALL_CONDITION_STATE_TYPE.E55", "TOPOGRAPHY_TYPE.E55")
  )) %>%
  mutate(OVERALL_CONDITION_STATE_TYPE.E55 = case_when(
    OVERALL_CONDITION_STATE_TYPE.E55 == "Good" ~ "Intact/No Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Excellent" ~ "Intact/No Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Fair" ~ "Slight Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Poor" ~ "Moderate Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Very Bad" ~ "Severe Damage",
    OVERALL_CONDITION_STATE_TYPE.E55 == "Unknown" ~ "No Visible/Accessible/Known",
    OVERALL_CONDITION_STATE_TYPE.E55 %in% c("n.a.", "na", "n.a", "na.", "NA") ~ "x",
    TRUE ~ OVERALL_CONDITION_STATE_TYPE.E55
  )) %>%
  rename(OVERALL_DAMAGE_SEVERITY_TYPE.E55 = OVERALL_CONDITION_STATE_TYPE.E55)



# zDisturbanceGroup : feuille demandée par la RCU
sortie_zDisturbanceGroup <- data_pivot %>%
  mutate(valeurs_ucop_origine = if_else(
    condition = valeurs_ucop_origine == "Erosion / Deterioration",
    true = "Erosion/Deterioration",
    false = valeurs_ucop_origine
  )) %>%
  filter(rcu_onglet == "zDisturbanceGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(DAMAGE_TREND_TYPE.E55 = "x",
         DISTURBANCE_CAUSE_CERTAINTY.I6 = "Definite",
         DISTURBANCE_DATE_FROM.E61 = "x",
         DISTURBANCE_DATE_TO.E61 = "x",
         DISTURBANCE_DATE_OCCURRED_ON.E61 = "x",
         DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41 = "UCOP Team") %>%
  mutate(DISTURBANCE_CAUSE_CATEGORY_TYPE.E55 = if_else(
    condition = is.na(DISTURBANCE_CAUSE_CATEGORY_TYPE.E55),
    true = "Unknown",
    DISTURBANCE_CAUSE_CATEGORY_TYPE.E55
  )) %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DAMAGE_TREND_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_CAUSE_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_FROM.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_TO.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_OCCURRED_ON.E61") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", 
                             variable_revue = "DISTURBANCE_DATE_OCCURRED_BEFORE.E61") %>%
  relocate(any_of(
    c("DISTURBANCE_CAUSE_CATEGORY_TYPE.E55", "EFFECT_TYPE.S9", "DAMAGE_TREND_TYPE.E55",
      "DISTURBANCE_CAUSE_TYPE.I4", "DISTURBANCE_CAUSE_CERTAINTY.I6", "DISTURBANCE_DATE_FROM.E61",
      "DISTURBANCE_DATE_TO.E61", "DISTURBANCE_DATE_OCCURRED_BEFORE.E61", "DISTURBANCE_DATE_OCCURRED_ON.E61",
      "DISTURBANCE_CAUSE_ASSIGNMENT_ASSESSOR_NAME.E41")
  ))


# ThreatGroup : feuille demandée par la RCU
sortie_ThreatGroup <- data_pivot %>%
  filter(rcu_onglet == "ThreatGroup") %>%
  pivot_wider(data = ., id_cols = ID, names_from = rcu_field_name, values_from = valeurs_ucop_origine) %>%
  select(-ID) %>%
  mutate(POTENTIAL_IMPACT_TYPE.E55 = "x",
         SECONDARY_THREAT_TYPE.E55 = "x",
         EXPOSURE.E62 = "x",
         RISK_EXTENT_TYPE.E55 = "x",
         RISK_SEVERITY_TYPE.E55 = "x",
         RISK_LEVEL.I4 = "x",
         RISK_LEVEL_CERTAINTY.I6 = "x",
         RISK_EVALUATION_ASSESSOR_NAME.E41 = "UCOP Team") %>%
  mutate(THREAT_TYPE.E55 = if_else(
    condition = is.na(THREAT_TYPE.E55),
    true = "Unknown",
    THREAT_TYPE.E55
  )) %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "POTENTIAL_IMPACT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "SECONDARY_THREAT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "EXPOSURE.E62") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_EXTENT_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_SEVERITY_TYPE.E55") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_LEVEL.I4") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_LEVEL_CERTAINTY.I6") %>%
  repetition_pattern_n_exact(x = ., variable_a_bon_pattern = "THREAT_TYPE.E55", 
                             variable_revue = "RISK_EVALUATION_ASSESSOR_NAME.E41") %>%
  relocate(VULNERABILITY_TYPE.E55, .after = EXPOSURE.E62) %>%
  relocate(THREAT_PROBABILITY_TYPE.E55, .after = VULNERABILITY_TYPE.E55)  %>%
  mutate(THREAT_PROBABILITY_TYPE.E55 = str_replace_all(string = THREAT_PROBABILITY_TYPE.E55, 
                                                       pattern = c("Possible" = "2 - Possible",
                                                                   "Probable" = "4 - Probable",
                                                                   "Unlikely" = "1 - Unlikely",
                                                                   "Likely" = "3 - Likely",
                                                                   "Certain" = "5 - Certain"))) %>%
  select(-VULNERABILITY_TYPE.E55)


# GeometryGroup : feuille demandée par la RCU
sortie_GeometryGroup <- heritage_features_points_gis %>%
  st_transform(x = ., crs = 4326) %>%
  select(geom) %>%
  mutate(GEOMETRIC_PLACE_EXPRESSION.SP5 = lwgeom::st_astext(geom)) %>%
  st_drop_geometry() %>%
  mutate(LOCATION_CERTAINTY.I6 = "Definite") %>%
  relocate(LOCATION_CERTAINTY.I6, .before = GEOMETRIC_PLACE_EXPRESSION.SP5)

# verification
tm_shape(st_as_sf(st_as_sfc(sortie_GeometryGroup$GEOMETRIC_PLACE_EXPRESSION.SP5))) + tm_dots()


# MeasurementGroup : feuille demandée par la RCU
sortie_MeasurementGroup <- heritage_features_points_tibble %>%
  select(Length, Width, Height) %>%
  mutate(Length = if_else(is.na(Length), "x", Length),
         Width = if_else(is.na(Width), "x", Width),
         Height = if_else(is.na(Height), "x", Height)) %>%
  mutate(MEASUREMENT_NUMBER.E60 = str_c(Length, Width, Height, sep = "|")) %>%
  mutate(MEASUREMENT_UNIT.E58 = "meters|meters|meters",
         DIMENSION_TYPE.E55 = "length|breadth/width|height",
         MEASUREMENT_SOURCE_TYPE.E55 = "Estimated|Estimated|Estimated") %>%
  select(-Length:-Height)


#### sorties par sheets dans un même fichier excel ####
list_of_datasets <- list("zzAssessment" = sortie_zzAssessment, 
                         "NameGroup" = sortie_NameGroup,
                         "GeometryGroup" = sortie_GeometryGroup,
                         "MeasurementGroup" = sortie_MeasurementGroup,
                         "DescriptionGroup" = sortie_DescriptionGroup,
                         "FeatureFormGroup" = sortie_FeatureFormGroup,
                         "InterpretationGroup" = sortie_InterpretationGroup,
                         "AdminAreasGroup" = sortie_AdminAreasGroup,
                         "NOT" = sortie_NOT,
                         "zDisturbanceGroup" = sortie_zDisturbanceGroup,
                         "ThreatGroup" = sortie_ThreatGroup)

openxlsx::write.xlsx(list_of_datasets, file = "sorties/finales/500_features_bulk_7/UCOP_heritage_features_bulk_points.xlsx")


