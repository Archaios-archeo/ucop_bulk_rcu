library(tidyverse)
library(readxl)
library(openxlsx)
library(sf)
library(shotGroups)
library(tmap)
tmap_mode("view")

# sources fonctions
source("bulk_functions.R")


#### Data sources ####
# general database
ucop_data_2019_2020_1 <- read_excel(path = "data/ucop_data_2019_2020_1_v3.xlsx")

# sélection des 500 premières features (versement par blocs)
ucop_data_500 <- ucop_data_2019_2020_1 %>%
  slice(1:500)

# dernière maj des données SIG, envoyées par VB (au 15/01/2019)
donnees_sig <- read_sf(dsn = "data/2021_01_features_general/export_2020_2_MAJ.shp", 
                       stringsAsFactors = FALSE)


#### heritage places ####
# combien d'hp dans bloc de 500 OS bdd
ucop_data_500 %>%
  filter(`Feature form` == "Multi-Component")

# les hp dans le SIG
heritage_place <- donnees_sig %>%
  left_join(x = ., y = ucop_data_500, by = c("FEATURE_ID" = "OS_Number")) %>%
  filter(`Feature form` == "Multi-Component")


# si différence, observation spatiale :
tm_shape(heritage_place %>% filter(OBJECTID_1 == 5646)) + tm_polygons() # un indéterminé mal géré dans le SIG

heritage_place <- heritage_place %>%
  filter(OBJECTID_1 != 5646)

## récupération des relations d'inclusions entre heritage features et heritage places (après tests diff. méthodes)
# méthode la plus robuste : buffer sur l'heritage place et fonction d'inclusion
hp_inclusion <- heritage_place %>%
  st_buffer(dist = 0.5) # on est en mètres, donc distance de 50 cm

# lien sig/bdd
donnees_sig_bdd <- donnees_sig %>%
  left_join(x = ., y = ucop_data_500, by = c("FEATURE_ID" = "OS_Number"))


hp_inclusion <- st_join(x = hp_inclusion %>% 
                          select(FEATURE_ID), # on ne garde que la géométrie et les identifiants
                        y = donnees_sig_bdd %>% 
                          select(FEATURE_ID, `Heritage Place ID`, `Survey unit`:Description) %>% # sélection des infos essentielles
                          filter(!is.na(FEATURE_ID)), # vire tous les éléments SIG qui ne sont pas lier à des entités bdd
                        join = st_contains) %>% # hp inclusion contains les données sig
  filter(FEATURE_ID.x != FEATURE_ID.y) # en virant au final l'auto-contains

# view sf object
hp_inclusion


## construction du tableau des relations (as RCU bulk upload)
tableau_des_relations_hp_os <- hp_inclusion %>%
  select(-`Heritage Place ID`:-Description) %>%
  rename(RESOURCEID_FROM = FEATURE_ID.x,
         RESOURCEID_TO = FEATURE_ID.y) %>%
  st_drop_geometry() %>%
  mutate(START_DATE = "x",
         END_DATE = "x",
         RELATION_TYPE = "is contained within / contains") %>%
  unique()

# sortie
write.xlsx(tableau_des_relations_hp_os, "sorties/finales/500_features_bulk_1/relations_features_places.xlsx", append = TRUE)


## création des données spatiales des heritage places
donnees_sig_heritage_place <- heritage_place %>%
  select(FEATURE_ID) %>%
  st_cast(., "POLYGON")

tm_shape(heritage_place) + tm_polygons()
tm_shape(donnees_sig_heritage_place) + tm_polygons()


# Create a minimal bounding box for each heritage feature,
# then create a tibble with width and height of each bounding box > as un output

heritage_identifiant <- seq(1, nrow(donnees_sig_heritage_place), 1)

bounding_box_tibble <- tibble(
  width = numeric(),
  height = numeric()
)
  

for (heritage_identifiant_a_traiter in heritage_identifiant) {
bounding_box <- donnees_sig_heritage_place %>%
  rowid_to_column() %>%
  filter(rowid == heritage_identifiant_a_traiter) %>%
  st_segmentize(., dfMaxLength = 2) %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename("point.x" = "X") %>%
  rename("point.y" = "Y") %>%
  shotGroups::getMinBBox()

bounding_box_sortie <- tibble(
  width = bounding_box$width,
  height = bounding_box$height
)

bounding_box_tibble <- bounding_box_tibble %>%
  bind_rows(bounding_box_sortie)

}

bounding_box_tibble <- bounding_box_tibble %>%
  mutate(width_ok = if_else(condition = width < height, true = width, false = height)) %>%
  mutate(length_ok = if_else(condition = width < height, true = height, false = width)) %>%
  select(width_ok, length_ok)

donnees_sig_heritage_place <- donnees_sig_heritage_place %>%
  bind_cols(bounding_box_tibble)

# sortie
st_write(donnees_sig_heritage_place, "sorties/finales/500_features_bulk_1/heritage_places_sig_2.gpkg", append = TRUE)


#### heritage features ####
### objectif : gestion des polygones "relation indéterminée" ###
# avoir une matrice des "touches" polygons murs avec les indéterminés,
# sachant qu'il faut au moins une boundary de lien et pas un point, see issue : http://github.com/r-spatial/sf/issues/234
murs_et_batis <- donnees_sig %>%
  left_join(x = ., y = ucop_data_500, by = c("FEATURE_ID" = "OS_Number")) %>%
  filter(`Feature form` %in% c("Wall", "Bank/Wall", "Bank/Earthwork") |
          featInterpretationType == "Terrace/Retaining Wall") # à tester initialement aussi avec les "Structure" > c'est-à-dire les buildings

# see and detect potential problems
tm_shape(murs_et_batis) + tm_polygons()

gestion_indetermines <- donnees_sig %>%
  filter(TYPE %in% c("Undetermined", "Undetermi*"))

murs_et_batis_summarise <- murs_et_batis %>%
  filter(TYPE %ni% c("Undetermined", "Undetermi*")) %>% # virer les polygones qui sont associés à des ID mais qui sont des indéterminés
  arrange(FEATURE_ID) %>% # reorder asc
  group_by(FEATURE_ID) %>% # group_by pour que les multipoly > poly si touches
  summarise() %>%
  ungroup()

tm_shape(murs_et_batis_summarise) + tm_polygons()


# intersection sur un côté et non uniquement sur un coin
murs_et_batis_sides <- murs_et_batis_summarise %>%
  st_join(., y = gestion_indetermines %>% 
            rowid_to_column() %>% 
            select(rowid), 
          join = st_queen) %>% # spectific function for sides and not corners
  filter(!is.na(rowid))

tm_shape(murs_et_batis_sides) + tm_polygons()


gestion_indetermines_500 <- gestion_indetermines %>%
  rowid_to_column() %>%
  select(rowid) %>%
  left_join(x = ., y = murs_et_batis_sides %>% 
              st_drop_geometry(), 
            by = "rowid") %>%
  filter(!is.na(FEATURE_ID))

relation <- murs_et_batis_sides %>%
  bind_rows(gestion_indetermines_500) %>%
  group_by(FEATURE_ID, rowid) %>%
  summarise(n = n()) %>%
  ungroup() %>% # il faut refaire un group_by ID pour que les murs qui ont 2 bouts avec des indé inclus à chaque fois
  group_by(FEATURE_ID) %>%
  summarise(n = n())

tm_shape(relation) + tm_polygons()

st_write(obj = relation, dsn = "sorties/intermediaires/intersect_indetermines_sides.gpkg", append = TRUE)


### heritage features : spatial data ###
donnees_sig_revues_500 <- donnees_sig %>%
  # sélection des 500 heritage features non recomposées
  left_join(x = ., y = ucop_data_500, by = c("FEATURE_ID" = "OS_Number")) %>% # jointure des 500 features traitées
  filter(!is.na(`Feature form`)) %>% # vire toutes les entités pas liées aux 500 traitées
  filter(`Feature form` != "Multi-Component") %>% # suppression des heritage places
  filter(`Feature form` %ni% c("Wall", "Bank/Wall", "Bank/Earthwork")) %>% #suppression des murs and co
  filter(featInterpretationType != "Terrace/Retaining Wall") %>% # suppression des murs and co
  filter(TYPE %ni% c("Undetermined", "Undetermi*")) %>%
  select(FEATURE_ID) %>%
  # ajout dans le tableau des murs and co. intégrant les polygones "relations indéterminées"
  bind_rows(relation %>% select(FEATURE_ID)) %>% # sélection seulement des identifiant
  # ajout dans le tableau des murs and co. qui ne sont pas liés à des polygones "relations indéterminées"
  bind_rows(murs_et_batis_summarise) %>%
  # virer les "doublons" en faisant un summarise pour unir les polygones selon l'ID
  group_by(FEATURE_ID) %>%
  summarise(n = n()) %>%
  ungroup()

tm_shape(donnees_sig_revues_500) + tm_polygons()

# contient des empty units ?
any(is.na(st_dimension(donnees_sig_revues_500)))

# if TRUE, lesquelles ?
donnees_sig_revues_500 %>%
  mutate(pas_de_geom = is.na(st_dimension(donnees_sig_revues_500))) %>%
  filter(pas_de_geom == TRUE)

# remove empty geometry (géom ponctuelles)
donnees_sig_revues_500 <- donnees_sig_revues_500 %>%
  mutate(pas_de_geom = is.na(st_dimension(donnees_sig_revues_500))) %>%
  filter(pas_de_geom == FALSE) %>%
  select(-pas_de_geom)

st_write(obj = donnees_sig_revues_500, dsn = "sorties/finales/500_features_bulk_1/donnees_spatiales_polygones_features.gpkg", append = TRUE)
