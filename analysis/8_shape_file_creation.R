library(sf)



# Carica i due shapefile
israel <- st_read("gadm41_ISR_shp/gadm41_ISR_0.shp")
palestine <- st_read("gadm41_PSE_shp/gadm41_PSE_0.shp")



# Unisci in un unico oggetto
combined <- rbind(israel, palestine)

plot(combined$geometry)

# Salva come un unico shapefile
st_write(combined$geometry, "israel_palestine_combined.shp")

