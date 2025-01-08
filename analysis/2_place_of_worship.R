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
  select(Name = name, Longitude, Latitude)

# Conversione da gradi a metri e kilometri
library(sf)
datasf <- st_as_sf(filtered_places2, coords = c("Longitude", "Latitude"),
                   crs = 4326)
datam <- st_transform(datasf, crs = 2039)
set.seed(123)
datakm <- st_coordinates(datam)/1000
datakm <- as.data.frame(datakm)
colnames(datakm) <- c("x", "y")

filtered_places2 <- data.frame(filtered_places2, datakm)


# Salvataggio dati in un csv
rownames(filtered_places2) <- NULL
filtered_places2$id <- 1:nrow(filtered_places2)
write.csv(filtered_places2, "places_of_worship_israel.csv", row.names = FALSE)

#### filtraggio e merge con il dataset

w_isr <- read.csv("places_of_worship_israel.csv")
data <- read.csv("IS_PAL.csv")
xy_j <- as.matrix(w_isr[, c("x", "y")])


library(foreach)
library(doParallel)

# Set up the parallel backend
num_cores <- round(parallel::detectCores()/2)  
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Parallel computation with foreach
min_distance_from_i <- foreach(i = 1:nrow(data), .combine = rbind, .packages = "base") %dopar% {
  # Extract coordinates for the i-th row of 'data'
  xy_i <- as.numeric(data[i, c("x", "y")])
  
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
colnames(min_distance_from_i) <- c("id_nearest_pow", "dist_nearest_pow")
write.csv(min_distance_from_i, "2_nearest_pow.csv", row.names = F)



   

