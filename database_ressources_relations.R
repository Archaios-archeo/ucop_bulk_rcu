library(tidyverse)
library(readxl)
library(openxlsx)
library(magick)
library(sf)
library(tmap)

tmap_mode("view")

# sources fonctions
source("bulk_functions.R")


#### DATA SOURCES ####
# general database
ucop_data_2019_2020_1 <- read_excel(path = "data/ucop_data_2019_2020_1_v5.xlsx")

ucop_data_500 <- ucop_data_2019_2020_1 %>%
  select(OS_Number, `People names`, `Feature form`, featInterpretationType, featFunctionType, `Description date`) %>%
  slice(2501:3000) %>%
  arrange(OS_Number)

# spatial data for spatial coordinates
heritage_place_gis <- st_read("sorties/finales/2019/heritage_places_2019.gpkg")

heritage_features_polygons_gis <- st_read("sorties/finales/500_features_bulk_6/donnees_spatiales_features_jg.gpkg", layer = "polygons")

heritage_features_lines_gis <- st_read("sorties/finales/500_features_bulk_6/donnees_spatiales_features_jg.gpkg", layer = "lines")

heritage_features_points_gis <- st_read("sorties/finales/500_features_bulk_6/donnees_spatiales_features_jg.gpkg", layer = "points")


# inputs : as lists
working_directory <- getwd()

#### FROM JPG TO PNG : photos ####

# specific directory
working_directory_photo <- paste0(working_directory, "/data/photos/features_places_bulk_5/")
working_directory_photo_JPG <- list.files(path = working_directory_photo, pattern = "JPG$")
working_directory_photo_jpg <- list.files(path = working_directory_photo, pattern = "jpg$")

# .JPG
liste_files <- list.files(path = working_directory_photo, pattern = "JPG$")

output_files_JPG <- gsub(".JPG", ".png", working_directory_photo_JPG)


# loop to convert every JPG file in png
for(i in 1:length(liste_files)){
  image_write(image_read(paste0(working_directory_photo, liste_files[i])),
              file.path(paste0(working_directory_photo, "new_image/"), output_files_JPG[i]))
}

# .jpg
liste_files <- list.files(path = working_directory_photo, pattern = "jpg$")

output_files_JPG <- gsub(".jpg", ".png", working_directory_photo_jpg)


# loop to convert every JPG file in png
for(i in 1:length(liste_files)){
  image_write(image_read(paste0(working_directory_photo, liste_files[i])),
              file.path(paste0(working_directory_photo, "new_image/"), output_files_JPG[i]))
}



#### DB AND PHOTOS RELATIONS ####
working_directory_photo <- paste0(working_directory, "/sorties/finales/500_features_bulk_6/photos/")
working_directory_photo_png <- list.files(path = working_directory_photo, pattern = "png$")


# tibble of photos names
liste_photos <- tibble(liste_photos_dossier = working_directory_photo_png) %>%
  mutate(OS_Number = str_sub(string = liste_photos_dossier, start = 1, end = 8))


relations <- liste_photos %>%
  left_join(., y = ucop_data_500 %>%
              mutate(dans_db = "oui"), 
            by = "OS_Number")

# each photo is related to an heritage place/feature ?
relations %>% filter(is.na(dans_db))


# each heritage place/feature got a photo ?
ucop_data_500 %>%
  left_join(., y = liste_photos, by = "OS_Number") %>%
  group_by_at(vars(-liste_photos_dossier)) %>%
  summarise_at("liste_photos_dossier", paste, collapse = "|") %>%
  filter(liste_photos_dossier == "NA")

# tibble of relations
tableau_des_relations_photos_feature_hp <- relations %>%
  select(-`People names`:-dans_db) %>%
  rename(RESOURCEID_FROM = liste_photos_dossier,
         RESOURCEID_TO = OS_Number) %>%
  mutate(START_DATE = "x",
         END_DATE = "x",
         RELATION_TYPE = "Heritage Resource - Information Resource",
         NOTES = str_c("photo of", RESOURCEID_TO, sep = " ")) %>%
  unique() %>%
  arrange(RESOURCEID_TO)

# sortie
tableau_des_relations_photos_feature_hp <- list("RELATIONS" = tableau_des_relations_photos_feature_hp)
openxlsx::write.xlsx(tableau_des_relations_photos_feature_hp, 
                     "sorties/finales/500_features_bulk_6/UCOP_relations_photos_features_places.xlsx")


#### BULK : PHOTOS ####
relations <- relations %>%
  select(liste_photos_dossier:`Description date`)


# creation of centroids from heritage places and heritages features for each photo of entity
relations_bulk <- relations %>%
  rename(FEATURE_ID = OS_Number) %>%
  left_join(., y = heritage_place_gis %>%
              st_centroid(.) %>%
              st_transform(x = ., crs = 4326) %>%
              select(geom, FEATURE_ID) %>%
              mutate(SPATIAL_COORDINATES_GEOMETRY.E47 = lwgeom::st_astext(geom)) %>%
              st_drop_geometry(),
            by = "FEATURE_ID") %>%
  bind_rows(relations %>%
              rename(FEATURE_ID = OS_Number) %>%
              left_join(., y = heritage_features_polygons_gis %>%
                st_centroid(.) %>%
                st_transform(x = ., crs = 4326) %>%
                mutate(SPATIAL_COORDINATES_GEOMETRY.E47 = lwgeom::st_astext(geom)) %>%
                st_drop_geometry(),
              by = "FEATURE_ID")
  ) %>%
  # bind_rows(relations %>%
  #             rename(FEATURE_ID = OS_Number) %>%
  #             left_join(., y = heritage_features_lines_gis %>%
  #                         st_centroid(.) %>%
  #                         st_transform(x = ., crs = 4326) %>%
  #                         mutate(SPATIAL_COORDINATES_GEOMETRY.E47 = lwgeom::st_astext(geom)) %>%
  #                         st_drop_geometry(),
  #                       by = "FEATURE_ID")
  # ) %>%
  # bind_rows(relations %>%
  #             rename(FEATURE_ID = OS_Number) %>%
  #             left_join(., y = heritage_features_points_gis %>%
  #                         st_transform(x = ., crs = 4326) %>%
  #                         filter(FEATURE_ID != "OS_01992") %>%
  #                         mutate(SPATIAL_COORDINATES_GEOMETRY.E47 = lwgeom::st_astext(geom)) %>%
  #                         st_drop_geometry(),
  #                       by = "FEATURE_ID")
  # ) %>%
  filter(!is.na(SPATIAL_COORDINATES_GEOMETRY.E47)) %>%
  arrange(FEATURE_ID) %>%
  unique()

## attention : bien vérifier qu'on ait les mêmes nb de lignes avec relations tout court


# vérification spatiale
tm_shape(st_as_sf(st_as_sfc(relations_bulk$SPATIAL_COORDINATES_GEOMETRY.E47))) + tm_dots()

relations_bulk <- relations_bulk %>%
  rowid_to_column(var = "ID")

# NOT : feuille demandée par la RCU
sortie_NOT <- relations_bulk %>%
  select(liste_photos_dossier, `Description date`, SPATIAL_COORDINATES_GEOMETRY.E47) %>%
  rename(CATALOGUE_ID.E42 = liste_photos_dossier,
         DATE_OF_ACQUISITION.E50 = `Description date`) %>%
  mutate(INFORMATION_RESOURCE_TYPE.E55 = "Photograph",
         INFORMATION_CARRIER_FORMAT_TYPE.E55 = "Digital Image",
         IMAGERY_SOURCE_TYPE.E55 = "Royal Commission for Al Ula (RCU)",
         IMAGERY_CREATOR_APPELLATION.E82 = "UCOP Team",
         IMAGERY_SAMPLED_RESOLUTION_TYPE.E55 = "Other/Unlisted",
         PROCESSING_TYPE.E55 = "Resized",
         IMAGERY_DATE_OF_PUBLICATION.E50 = as.character(Sys.Date()),
         RIGHT_TYPE.E55 = "Copyright (All Rights Reserved)",
         DESCRIPTION.E62 = "x") %>%
  relocate(CATALOGUE_ID.E42, .after = INFORMATION_CARRIER_FORMAT_TYPE.E55) %>%
  relocate(DATE_OF_ACQUISITION.E50, .before = IMAGERY_DATE_OF_PUBLICATION.E50) %>%
  relocate(SPATIAL_COORDINATES_GEOMETRY.E47, .after = RIGHT_TYPE.E55)



# à simplifier avec fonction générale
ligne_precedente <- seq(1, nrow(sortie_NOT), 1) - 1
ligne_precedente[1] <- 1

for (i in 1:nrow(sortie_NOT)) {
  sortie_NOT <- sortie_NOT %>%
    mutate(DATE_OF_ACQUISITION.E50 = if_else(
      condition = nchar(DATE_OF_ACQUISITION.E50) != 10,
      true = DATE_OF_ACQUISITION.E50[ligne_precedente],
      false = DATE_OF_ACQUISITION.E50
    ))
}


# ImageDetails : feuille demandée par la RCU
sortie_ImageDetails <- relations_bulk %>%
  select(liste_photos_dossier) %>%
  mutate(IMAGERY_PLATFORM_TYPE.E55 = "Hand-operated (Ground)",
         IMAGERY_SENSOR_TYPE.E55 = "Unknown",
         IMAGERY_BANDS_TYPE.E55 = "Colour",
         IMAGERY_CAMERA_SENSOR_TYPE.E55 = "Digital",
         IMAGERY_CAMERA_SENSOR_RESOLUTION_TYPE.E55 = "Unknown") %>%
  select(-liste_photos_dossier)


# ImageGroup : feuille demandée par la RCU
sortie_ImageGroup <- relations_bulk %>%
  select(liste_photos_dossier) %>%
  rename(FILE_PATH.E62 = liste_photos_dossier) %>%
  mutate(THUMBNAIL.E62 = str_c("thumb_", FILE_PATH.E62))



## sorties par sheets dans un même fichier excel
list_of_datasets <- list("ImageDetails" = sortie_ImageDetails, 
                         "NOT" = sortie_NOT,
                         "ImageGroup" = sortie_ImageGroup)

openxlsx::write.xlsx(list_of_datasets, 
           file = "sorties/finales/500_features_bulk_6/UCOP_ressources_photos.xlsx")



#### DATA SKETCHES ####

# specific directory
working_directory_sketch <- paste0(working_directory, "/data/sketches/features_places_bulk_1/")
working_directory_sketch_jpeg <- list.files(path = working_directory_sketch, pattern = "jpeg$")
# il y a aussi du .png
working_directory_sketch_png <- list.files(path = working_directory_sketch, pattern = "png$")

# vérification du nb sketches (équivalent à nb sketches dans dossier, au cas où autre format)
length(working_directory_sketch_jpeg) + length(working_directory_sketch_png)


#### FROM JPEG TO PNG ####
# .jpeg
liste_files <- list.files(path = working_directory_sketch, pattern = "jpeg$")

output_files_JPEG <- gsub(".jpeg", ".png", working_directory_sketch_jpeg)


# loop to convert every JPG file in png
for(i in 1:length(liste_files)){
  image_write(image_read(paste0(working_directory_sketch, liste_files[i])),
              file.path(paste0(working_directory_sketch, "new_image/"), output_files_JPEG[i]))
}


#### DB AND SKETCHES RELATIONS ####
working_directory_sketch <- paste0(working_directory, "/sorties/finales/500_features_bulk_1/sketches/")
working_directory_sketch_png <- list.files(path = working_directory_sketch, pattern = "png$")

# table des relations des sketches avec renvois
renvois_relations_sketches <- read_excel(path = "data/sketches/sketches_gestion.xlsx", sheet = "Tableau_des_relations")

# tibble of photos names
liste_sketches <- tibble(liste_sketches_dossier = working_directory_sketch_png) %>%
  mutate(OS_Number = str_sub(string = liste_sketches_dossier, start = 1, end = 8))


relations <- liste_sketches %>%
  # intégration des features qui n'ont à la base pas de sketches mais qui sont quand mm présentes sur d'autres
  left_join(., y = renvois_relations_sketches, by = c("OS_Number" = "ID_sketch")) %>%
  filter(!is.na(ID_OS_renvoi)) %>%
  mutate(OS_Number = str_c(OS_Number, ID_OS_renvoi, sep = "|")) %>%
  select(-ID_OS_renvoi) %>%
  separate_rows(OS_Number, sep = "\\|") %>%
  # ajout du tibble de liste
  bind_rows(liste_sketches) %>%
  # sélection tableau individus uniques
  unique() %>%
  # jointure avec les 500 features sur lesquelles on travaille
  arrange(OS_Number) %>% # tri croissant pour meilleure visualisation personnelle
  left_join(., y = ucop_data_500 %>%
              mutate(dans_db = "oui"), 
            by = "OS_Number")


# each sketch is related to an heritage place/feature ?
relations %>% filter(is.na(dans_db))


# each heritage place/feature got a photo ?
ucop_data_500 %>%
  left_join(., y = liste_sketches, by = "OS_Number") %>%
  group_by_at(vars(-liste_sketches_dossier)) %>%
  filter(is.na(liste_sketches_dossier))


# tibble of relations
tableau_des_relations_sketches_feature_hp <- relations %>%
  select(-`People names`:-dans_db) %>%
  rename(RESOURCEID_FROM = liste_sketches_dossier,
         RESOURCEID_TO = OS_Number) %>%
  mutate(START_DATE = "x",
         END_DATE = "x",
         RELATION_TYPE = "Heritage Resource - Information Resource",
         NOTES = str_c("sketch of", RESOURCEID_TO, sep = " ")) %>%
  unique()

# sortie
tableau_des_relations_sketches_feature_hp <- list("RELATIONS" = tableau_des_relations_sketches_feature_hp)
openxlsx::write.xlsx(tableau_des_relations_sketches_feature_hp, 
                     "sorties/finales/500_features_bulk_1/UCOP_relations_sketches_features_places.xlsx", 
                     append = TRUE)



#### BULK : SKETCHES ####
# creation of centroids from heritage places and heritages features for each sketch
relations_bulk <- relations %>%
  rename(FEATURE_ID = OS_Number) %>%
  left_join(., y = heritage_place_gis %>%
              st_centroid(.) %>%
              st_transform(x = ., crs = 4326) %>%
              select(geom, FEATURE_ID) %>%
              mutate(SPATIAL_COORDINATES_GEOMETRY.E47 = lwgeom::st_astext(geom)) %>%
              st_drop_geometry(),
            by = "FEATURE_ID") %>%
  bind_rows(relations %>%
              rename(FEATURE_ID = OS_Number) %>%
              left_join(., y = heritage_features_polygons_gis %>%
                          st_centroid(.) %>%
                          st_transform(x = ., crs = 4326) %>%
                          mutate(SPATIAL_COORDINATES_GEOMETRY.E47 = lwgeom::st_astext(geom)) %>%
                          st_drop_geometry(),
                        by = "FEATURE_ID")
  ) %>%
  bind_rows(relations %>%
              rename(FEATURE_ID = OS_Number) %>%
              left_join(., y = heritage_features_lines_gis %>%
                          st_centroid(.) %>%
                          st_transform(x = ., crs = 4326) %>%
                          mutate(SPATIAL_COORDINATES_GEOMETRY.E47 = lwgeom::st_astext(geom)) %>%
                          st_drop_geometry(),
                        by = "FEATURE_ID")
  ) %>%
  bind_rows(relations %>%
              rename(FEATURE_ID = OS_Number) %>%
              left_join(., y = heritage_features_points_gis %>%
                          st_transform(x = ., crs = 4326) %>%
                          mutate(SPATIAL_COORDINATES_GEOMETRY.E47 = lwgeom::st_astext(geom)) %>%
                          st_drop_geometry(),
                        by = "FEATURE_ID")
  ) %>%
  filter(!is.na(SPATIAL_COORDINATES_GEOMETRY.E47)) %>%
  arrange(FEATURE_ID)

## attention : bien vérifier qu'on ait les mêmes nb de lignes avec relations tout court


# vérification spatiale
tm_shape(st_as_sf(st_as_sfc(relations_bulk$SPATIAL_COORDINATES_GEOMETRY.E47))) + tm_dots()

relations_bulk <- relations_bulk %>%
  rowid_to_column(var = "ID")

# NOT : feuille demandée par la RCU (note : attention, il y a plus de relations que de sketches)
sortie_NOT <- relations_bulk %>%
  rename(CATALOGUE_ID.E42 = liste_sketches_dossier,
         DATE_OF_ACQUISITION.E50 = `Description date`) %>%
  mutate(INFORMATION_RESOURCE_TYPE.E55 = "Drawing/Reconstruction",
         INFORMATION_CARRIER_FORMAT_TYPE.E55 = "Digital Image",
         IMAGERY_SOURCE_TYPE.E55 = "Royal Commission for Al Ula (RCU)",
         IMAGERY_CREATOR_APPELLATION.E82 = "UCOP Team",
         IMAGERY_SAMPLED_RESOLUTION_TYPE.E55 = "Other/Unlisted",
         PROCESSING_TYPE.E55 = "Not Applicable",
         IMAGERY_DATE_OF_PUBLICATION.E50 = as.character(Sys.Date()),
         RIGHT_TYPE.E55 = "Copyright (All Rights Reserved)",
         DESCRIPTION.E62 = str_c("sketch of a ", str_to_lower(`Feature form`), ": ", str_to_lower(featInterpretationType), sep = "")) %>%
  select(-FEATURE_ID:-featFunctionType, -dans_db, -n, -ID) %>%
  relocate(CATALOGUE_ID.E42, .after = INFORMATION_CARRIER_FORMAT_TYPE.E55) %>%
  relocate(DATE_OF_ACQUISITION.E50, .before = IMAGERY_DATE_OF_PUBLICATION.E50) %>%
  relocate(SPATIAL_COORDINATES_GEOMETRY.E47, .after = RIGHT_TYPE.E55) %>%
  mutate(duplicata_png = duplicated(x = CATALOGUE_ID.E42)) %>%
  filter(duplicata_png == FALSE) %>%
  select(-duplicata_png)
  
# à simplifier avec fonction générale
ligne_precedente <- seq(1, nrow(sortie_NOT), 1) - 1
ligne_precedente[1] <- 1

for (i in 1:nrow(sortie_NOT)) {
  sortie_NOT <- sortie_NOT %>%
    mutate(DATE_OF_ACQUISITION.E50 = if_else(
      condition = nchar(DATE_OF_ACQUISITION.E50) != 10 | is.na(DATE_OF_ACQUISITION.E50),
      true = as.character(DATE_OF_ACQUISITION.E50[ligne_precedente]),
      false = DATE_OF_ACQUISITION.E50
    ))
}


# ImageDetails : feuille demandée par la RCU
sortie_ImageDetails <- relations_bulk %>%
  mutate(duplicata_png = duplicated(x = liste_sketches_dossier)) %>%
  filter(duplicata_png == FALSE) %>%
  select(ID) %>%
  mutate(IMAGERY_PLATFORM_TYPE.E55 = "Hand-operated (Ground)",
         IMAGERY_SENSOR_TYPE.E55 = "Not Applicable",
         IMAGERY_BANDS_TYPE.E55 = "Not Applicable",
         IMAGERY_CAMERA_SENSOR_TYPE.E55 = "Not Applicable",
         IMAGERY_CAMERA_SENSOR_RESOLUTION_TYPE.E55 = "Not Applicable") %>%
  select(-ID)


# ImageGroup : feuille demandée par la RCU
sortie_ImageGroup <- relations_bulk %>%
  mutate(duplicata_png = duplicated(x = liste_sketches_dossier)) %>%
  filter(duplicata_png == FALSE) %>%
  select(liste_sketches_dossier) %>%
  rename(FILE_PATH.E62 = liste_sketches_dossier) %>%
  mutate(THUMBNAIL.E62 = str_c("thumb_", FILE_PATH.E62))



## sorties par sheets dans un même fichier excel
list_of_datasets <- list("ImageDetails" = sortie_ImageDetails, 
                         "NOT" = sortie_NOT,
                         "ImageGroup" = sortie_ImageGroup)

write.xlsx(list_of_datasets, 
           file = "sorties/finales/500_features_bulk_1/UCOP_ressources_sketches.xlsx", 
           append = TRUE)





