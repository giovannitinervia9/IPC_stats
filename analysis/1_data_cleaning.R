rm(list = ls())

# caricamento librerie
if(!require(tidyverse)) install.packages("tidyverse")

ged241 <- readRDS("ged241.rds")

data <- ged241 |> 
  filter(conflict_name == "Israel: Palestine" & country == "Israel") |> 
  select(id, year, side_b, adm_1, longitude, latitude, event_clarity, date_prec,
         where_prec, date_start, date_end, deaths_a, deaths_b, deaths_civilians,
         deaths_unknown, best) |> 
  filter(date_prec < 4 & where_prec < 3 & event_clarity == 1)

data_non_filtered <- ged241 |> 
  filter(conflict_name == "Israel: Palestine" & country == "Israel") |> 
  select(id, year, side_b, adm_1, longitude, latitude, event_clarity, date_prec,
         where_prec, date_start, date_end, deaths_a, deaths_b, deaths_civilians,
         deaths_unknown, best)
write.csv(data_non_filtered, "IS_PAL_non_filtered.csv", row.names = FALSE)

# date_prec = 3 -> week known
# where_prec < 2 -> radius of 25 km

table(data$date_prec, data$where_prec) |> prop.table()

df <- data.frame(x = war$longitude, y = war$latitude, y = war$days_since_start)

# conversione da gradi a metri
library(sf)
datasf <- st_as_sf(data, coords = c("longitude", "latitude"),
                   crs = 4326)
datam <- st_transform(datasf, crs = 2039) # 2039 da sostituire al 32633
set.seed(123)
datakm <- (st_coordinates(datam) + rnorm(nrow(data), 0, 1))/1000
datakm <- as.data.frame(datakm)
colnames(datakm) <- c("x", "y")

data <- data.frame(data, datakm)

data <- data[order(data$date_start, data$x, data$y), ]

data$event_duration <- as.numeric((data$date_end - data$date_start)/(3600*24))

date_start <- data$date_start
date_start <- as.Date(date_start, format = "%Y-%m-%d")
days_since_start <- as.numeric(date_start - min(date_start))
data$days_since_start <- days_since_start
# plot(data$days_since_start)
data$intertimes <- c(NA, diff(data$days_since_start))
head(data)
write.csv(data, "IS_PAL.csv", row.names = F)

