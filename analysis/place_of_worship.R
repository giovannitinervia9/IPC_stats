rm(list = ls())
library(osmdata)
library(sf)
library(tidyverse)

# Definisci l'area geografica (esempio: Israele)
area2 <- "Israel"

# Query per luoghi di culto
places_of_worship_i <- opq(bbox = area2) |>
  add_osm_feature(key = "amenity", value = "place_of_worship") |>
  osmdata_sf()

# Estrai i dati dei punti (nodi)
points2 <- places_of_worship_i$osm_points |>
  select(name, geometry) |>
  mutate(Latitude = st_coordinates(geometry)[,2],
         Longitude = st_coordinates(geometry)[,1]) |>
  st_drop_geometry() # Rimuove la colonna geometrica, se non necessaria

# Rimuovi i punti senza nome
filtered_places2 <- points2 |>
  filter(!is.na(name)) |>
  select(Name = name, Latitude, Longitude)

# Salvataggio dati in un csv
write.csv(filtered_places2, "places_of_worship_israel.csv", row.names = FALSE)