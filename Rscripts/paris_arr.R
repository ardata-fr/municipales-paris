library(dplyr)
library(sf)
library(rmapshaper)

source("Rscripts/as_secteur.R")

Arr <- st_read("https://opendata.paris.fr/explore/dataset/arrondissements/download/?format=geojson&timezone=Europe/Berlin&lang=fr")
Arr <- st_transform(Arr,crs=2154)

paris_arr <- ms_simplify(Arr,keep = 0.2)
paris_arr <- mutate(paris_arr, secteur = as_secteur(c_ar))
paris_arr <- paris_arr %>% 
  group_by(secteur) %>% 
  summarise(l_aroff = paste0(l_aroff, collapse = ", "), .groups = "drop")

saveRDS(paris_arr, file = "data/paris_arr.RDS")

