library(tidyverse)
library(readxl)
library(openxlsx)
library(sf)
library(shotGroups)
library(tmap)
tmap_mode("view")

# sources fonctions
source("bulk_functions.R")


#### DATA SOURCES ####
# general database
ucop_data_2020_2 <- read_excel(path = "data/BDD_OS_2020_2_v4.xlsx") %>%
  arrange(OS_Number)

# dernière MAJ des données SIG, envoyées par GG (au 18/03/2021)
donnees_sig <- read_sf(dsn = "data/2021_01_features_general/export_2020_2_MAJ.shp", 
                       stringsAsFactors = FALSE) %>%
  st_transform(crs = 32637)

# dernière MAJ des données ponctuelles SIG, envoyées par VB (au 19/03/2021)
donnees_sig_points <- st_read(dsn = "data/2021_01_features_general/feature_point.shp", 
                              stringsAsFactors = FALSE) %>%
  st_transform(crs = 32637)

# dernière MAJ des données linéaires SIG, VB (au 19/03/2021)
donnees_sig_lines <- st_read(dsn = "data/2021_01_features_general/feature_line.shp", 
                              stringsAsFactors = FALSE) %>%
  st_transform(crs = 32637)



#### HERITAGE PLACES ####
# quelles d'hp sur 2020.1
hp_2020_2_bdd <- ucop_data_2020_2 %>%
  filter(`Form arrangement` == "Clustered (Heritage Place)")

# les hp dans le SIG : creating data
heritage_place <- donnees_sig %>%
  left_join(x = ., y = ucop_data_2020_2, by = c("FEATURE_ID" = "OS_Number")) %>%
  filter(`Form arrangement` == "Clustered (Heritage Place)")


# Pas dans SIG:
hp_2020_2_bdd %>%
  left_join(x = ., y = donnees_sig %>%
              select(FEATURE_ID), by = c("OS_Number" = "FEATURE_ID")) %>%
  st_as_sf(.) %>%
  filter(st_is_empty(.)) %>%
  view()
# il y en a 7 : essentiellement des qanats, donc à discuter avec Julien et Gaël du pourquoi du comment :)


#### verification process ####
### Si plusieurs polygones associés à des heritage places : lesquelles ? ###
id_hp_duplicated <- heritage_place %>%
  mutate(duplication = duplicated(x = FEATURE_ID)) %>%
  filter(duplication == TRUE) %>%
  select(FEATURE_ID, duplication) %>%
  st_drop_geometry()

# data to visualisize these polygons
heritage_place_duplicated <- heritage_place %>%
  select(OBJECTID_1, FEATURE_ID, TYPE) %>%
  left_join(., y = id_hp_duplicated, by = "FEATURE_ID") %>% 
  filter(duplication == TRUE) %>%
  # if there is empty polygon, suppress
  mutate(pas_de_geom = is.na(st_dimension(.))) %>%
  filter(pas_de_geom == FALSE)
  
tm_shape(heritage_place_duplicated) + tm_polygons()


# if necessary : suppress wrong objectID
heritage_place <- heritage_place %>%
  filter(OBJECTID_1 %ni% c("10127", "12052", "12053", "12486"))


# list of BDD heritage places not contain in GIS polygon data
no_spatial_hp_data <- hp_2020_2_bdd %>%
  select(OS_Number) %>%
  left_join(., y = heritage_place %>% 
              select(FEATURE_ID) %>%
              mutate(sig = "oui") %>%
              st_drop_geometry(),
            by = c("OS_Number" = "FEATURE_ID")) %>%
  filter(is.na(sig))

write.xlsx(x = no_spatial_hp_data, file = "sorties/intermediaires/2020_2_pas_sig_heritage_place.xlsx", append = TRUE)
rm(heritage_place_duplicated, id_hp_duplicated, no_spatial_hp_data)

#### inclusion relations between heritage places and heritages features ####
## récupération des relations d'inclusions entre heritage features et heritage places (après tests diff. méthodes)
# méthode la plus robuste : buffer sur l'heritage place et fonction d'inclusion
hp_inclusion <- heritage_place %>%
  st_buffer(dist = 0.5) # on est en mètres, donc distance de 50 cm

# lien buffer et requête sur base (prise en considération aussi de 2020-1 si création ensuite)
donnees_sig_complete_bdd <- donnees_sig %>%
  left_join(x = ., y = ucop_data_2020_2, by = c("FEATURE_ID" = "OS_Number"))


hp_inclusion <- st_join(x = hp_inclusion %>% 
                          select(FEATURE_ID), # on ne garde que la géométrie et les identifiants
                        y = donnees_sig_complete_bdd %>% 
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
write.xlsx(tableau_des_relations_hp_os, "sorties/finales/2020_2/2020_2_relations_features_places.xlsx")

rm(hp_inclusion, donnees_sig_complete_bdd, tableau_des_relations_hp_os)


#### heritage places with width and length ####
heritage_place_simple_polygon <- heritage_place %>%
  select(FEATURE_ID) %>%
  st_cast(., "POLYGON") # transform as polygon

tm_shape(heritage_place) + tm_polygons()
tm_shape(heritage_place_simple_polygon) + tm_polygons()


# Length and width calculations :
# Create a minimal bounding box for each heritage place,
# then create a tibble with width and height of each bounding box > as un output

heritage_identifiant <- seq(1, nrow(heritage_place_simple_polygon), 1)

bounding_box_tibble <- tibble(
  width = numeric(),
  height = numeric()
)
  

for (heritage_identifiant_a_traiter in heritage_identifiant) {
bounding_box <- heritage_place_simple_polygon %>%
  rowid_to_column() %>%
  filter(rowid == heritage_identifiant_a_traiter) %>%
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
  mutate(length_ok = if_else(condition = width < height, true = height, false = width)) %>%
  mutate(width_ok = if_else(condition = width < height, true = width, false = height)) %>%
  select(length_ok, width_ok)

heritage_place_simple_polygon <- heritage_place_simple_polygon %>%
  bind_cols(bounding_box_tibble)


# sortie
st_write(heritage_place_simple_polygon, "sorties/finales/2020_2/heritage_places_2020_2.gpkg")


rm(bounding_box_sortie, bounding_box_tibble, bounding_box, heritage_place)


#### HERITAGE FEATURES ####
# sélection des 500 premières features (versement par blocs)
ucop_data_500 <- ucop_data_2020_2 %>%
  slice(0500:1000) %>% # not 501 because suppression of a row before
  arrange(OS_Number) %>%
  filter(`Form arrangement` != "Clustered (Heritage Place)")


#### gestion des polygones "relation indéterminée" ####
# avoir une matrice des "touches" polygons murs avec les indéterminés,
# sachant qu'il faut au moins une boundary de lien et pas un point, see issue : http://github.com/r-spatial/sf/issues/234
murs_et_batis <- donnees_sig %>%
  left_join(x = ., y = ucop_data_500, by = c("FEATURE_ID" = "OS_Number")) %>%
  filter(`Feature form` %in% c("Wall", "Bank/Wall", "Bank/Earthwork") |
          featInterpretationType == "Terrace/Retaining Wall")

# see and detect potential problems
tm_shape(murs_et_batis) + tm_polygons()

# there is empty polygons: les virer
murs_et_batis <- murs_et_batis %>%
  filter(FEATURE_ID != "OS_07513") %>%
  filter(OBJECTID_1 != "10433")
  

gestion_indetermines <- donnees_sig %>%
  filter(TYPE %in% c("Undetermined", "Undetermi*"))

murs_et_batis_summarise <- murs_et_batis %>%
  filter(TYPE %ni% c("Undetermined", "Undetermi*")) %>% # virer les polygones qui sont associés à des ID mais qui sont des indéterminés
  arrange(FEATURE_ID) %>% # reorder asc
  group_by(FEATURE_ID) %>% # group_by pour que les multipoly > poly si touches
  summarise() %>%
  ungroup()

tm_shape(st_make_valid(x = murs_et_batis_summarise)) + tm_polygons()


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

st_write(obj = relation, dsn = "sorties/intermediaires/500_features_bulk_12/intersect_indetermines_sides.gpkg", append=FALSE)
# probably need review on GIS (see documentation "polygones_indetermines_unis_heritage_feature.docx")

relation_revues <- st_read(dsn = "sorties/intermediaires/500_features_bulk_12/intersect_indetermines_sides_jg.gpkg") %>%
  st_transform(crs = 32637)

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
  bind_rows(relation_revues %>% select(FEATURE_ID) %>% # sélection seulement des identifiants
              rename(geometry = geom)) %>% # il a planté... la geometry s'est transformé en geom avec la transfo gpkg
  # ajout dans le tableau des murs and co. qui ne sont pas liés à des polygones "relations indéterminées"
  bind_rows(murs_et_batis_summarise) %>%
  # virer les "doublons" en faisant un summarise pour unir les polygones selon l'ID
  group_by(FEATURE_ID) %>%
  summarise(n = n()) %>%
  ungroup()

tm_shape(donnees_sig_revues_500) + tm_polygons() +tmap_options(check.and.fix = TRUE)
# problème de validité d'un polygone : il s'agit d'une OS complexe (un plot)
donnees_sig_revues_500 <- donnees_sig_revues_500 %>%
  st_make_valid(.)

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


st_write(obj = donnees_sig_revues_500, dsn = "sorties/finales/500_features_bulk_12/donnees_spatiales_features.gpkg", 
         layer = "polygons", append=FALSE)


#### transformation des points en gpkg ####
donnees_sig_points <- donnees_sig_points %>%
  left_join(x = ., y = ucop_data_500 %>% 
              mutate(ucop_500 = "oui"), 
            by = c("FEATURE_ID" = "OS_Number")) %>%
  filter(ucop_500 == "oui") %>%
  select(FEATURE_ID)

st_write(obj = donnees_sig_points, dsn = "sorties/finales/500_features_bulk_12/donnees_spatiales_features.gpkg", 
         layer = "points", append=TRUE)


#### transformation des lines en gpkg ####
donnees_sig_lines <- donnees_sig_lines %>%
  left_join(x = ., y = ucop_data_500 %>% 
              mutate(ucop_500 = "oui"), 
            by = c("FEATURE_ID" = "OS_Number")) %>%
  filter(ucop_500 == "oui") %>%
  select(FEATURE_ID) %>%
  # attention, si et seulement si les multilinstring sont de traditionnelles polylignes continues
  # si non, transformer en réel multilinestring car cela a du sens
  st_cast(., "LINESTRING")

# donnees_sig_lines <- donnees_sig_lines %>%
#   filter(FEATURE_ID %in% c("OS_04274", "OS_04258")) %>%
#   group_by(FEATURE_ID) %>%
#   summarise() %>%
#   st_cast("MULTILINESTRING")

st_write(obj = donnees_sig_lines, dsn = "sorties/finales/500_features_bulk_9/donnees_spatiales_features.gpkg",
         layer = "lines", append=TRUE) # note: pour B6 > les linestrings ne correspondent plus à rien (non ajustement GIS avec DB)


