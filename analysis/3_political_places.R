rm(list = ls())

# Carica i pacchetti necessari
library(osmdata)
library(sf)
library(tidyverse)

# Definisci l'area geografica (esempio: Israele)
area2 <- "Israel"

# Query per luoghi di rilevanza socio-politica (es. uffici governativi, ambasciate)
political_places <- opq(bbox = area2) |>
  add_osm_feature(key = "amenity", value = c("embassy", "townhall")) |>  # Ambasciate e municipi
  osmdata_sf()

# Estrai i dati dei punti (nodi)
political_points <- political_places$osm_points |>
  select(name, geometry) |>
  mutate(Latitude = st_coordinates(geometry)[,2],
         Longitude = st_coordinates(geometry)[,1]) |>
  st_drop_geometry() # Rimuove la colonna geometrica, se non necessaria

# Rimuovi i punti senza nome
filtered_political <- political_points |>
  filter(!is.na(name)) |>
  select(Name = name, Latitude, Longitude)

# Salva i dati in un CSV
rownames(filtered_political) <- NULL
filtered_political$id <- 1:nrow(filtered_political)
write.csv(filtered_political, "political_places_israel.csv", row.names = FALSE)

# Stampa anteprima del risultato
head(filtered_political)


#### filtraggio e merge con il dataset

p_isr <- read.csv("political_places_israel.csv")
data <- read.csv("IS_PAL.csv")
xy_j <- as.matrix(p_isr[, c("Latitude", "Longitude")])


library(foreach)
library(doParallel)

# Set up the parallel backend
num_cores <- round(parallel::detectCores()/2)  
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Parallel computation with foreach
min_distance_from_i <- foreach(i = 1:nrow(data), .combine = rbind, .packages = "base") %dopar% {
  # Extract coordinates for the i-th row of 'data'
  xy_i <- as.numeric(data[i, c("latitude", "longitude")])
  
  distance_from_i <- apply(sweep(xy_j, 2, xy_i, "-"), 1, function(x) sum(sqrt(x^2)))
  
  # Return the index of the minimum distance
  c(which.min(distance_from_i), min(distance_from_i))
}

# Stop the parallel backend
stopCluster(cl)

nrow(min_distance_from_i)

rownames(min_distance_from_i) <- NULL
min_distance_from_i <- as.data.frame(min_distance_from_i)
head(min_distance_from_i)
colnames(min_distance_from_i) <- c("id_nearest_gov", "dist_nearest_gov")
write.csv(min_distance_from_i, "3_nearest_gov.csv", row.names = F)





