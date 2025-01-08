# Pulisci l'ambiente
rm(list = ls())

# Carica i pacchetti necessari
library(osmdata)
library(sf)
library(tidyverse)

# Definisci l'area geografica (esempio: Israele)
area2 <- "Israel"

# Query per luoghi di rilevanza militare
military_places <- opq(bbox = area2) |>
  add_osm_feature(key = "military", value = c("checkpoint")) |>  # Punti di controllo
  osmdata_sf()

# Estrai i dati dei punti (nodi)
military_points <- military_places$osm_points |>
 select(name, geometry) |>
  mutate(Latitude = st_coordinates(geometry)[,2],
         Longitude = st_coordinates(geometry)[,1]) |>
  st_drop_geometry() # Rimuove la colonna geometrica, se non necessaria


# Conversione da gradi a metri e kilometri
library(sf)
datasf <- st_as_sf(military_points, coords = c("Longitude", "Latitude"),
                   crs = 4326)
datam <- st_transform(datasf, crs = 2039)
set.seed(123)
datakm <- st_coordinates(datam)/1000
datakm <- as.data.frame(datakm)
colnames(datakm) <- c("x", "y")

military_points <- data.frame(military_points, datakm)


# Salva i dati in un CSV
rownames(military_points) <- NULL
military_points$id <- 1:nrow(military_points)
military_points <- military_points[, c("name", "Longitude", "Latitude", "id", "x", "y")]
write.csv(military_points, "military_checkpoints_israel.csv", row.names = FALSE)

# Anteprima 
head(military_points)


#### filtraggio e merge con il dataset
rm(list = ls())
c_isr <- read.csv("military_checkpoints_israel.csv")
data <- read.csv("IS_PAL.csv")
xy_j <- as.matrix(c_isr[, c("x", "y")])
head(c_isr)

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
colnames(min_distance_from_i) <- c("id_nearest_chp", "dist_nearest_chp")
head(min_distance_from_i)
write.csv(min_distance_from_i, "4_nearest_chp.csv", row.names = F)

