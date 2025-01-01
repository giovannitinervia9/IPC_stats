rm(list = ls())
# caricamento librerie
if(!require(tidyverse)) install.packages("tidyverse")
# if(!require(maps)) install.packages("maps")
# if(!require(mapproj)) install.packages("mapproj")
# if(!require(colorspace)) install.packages("colorspace")
# if(!require(latex2exp)) install.packages("latex2exp")
# if(!require(tmaptools)) install.packages("tmaptools")
if(!require(sf)) install.packages("sf")
if(!require(sp)) install.packages("sp")
if(!require(stopp)) install.packages("stopp")
if(!require(spatstat)) install.packages("spatstat")
# if(!require(rnaturalearth)) install.packages("rnaturalearth")
# if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata")

# caricamento dati
war <- read.csv("war.csv")
war$side_b <- factor(war$side_b)
war$adm_1 <- factor(war$adm_1)
war$event_clarity <- factor(war$event_clarity)
war$date_prec <- factor(war$date_prec)
colnames(war)



# intertimes_table <- table(war$intertimes[-1])
# intertimes <- as.integer(names(intertimes_table))
# n_total <- sum(intertimes_table)
# abs_freq <- as.vector(intertimes_table)
# rel_freq <- abs_freq/n_total
# cum_freq <- cumsum(rel_freq)
# surv <- (1 - cum_freq) + rel_freq
# intertimes_distr <- data.frame(intertimes = intertimes, abs_freq = abs_freq, 
#                                rel_freq = rel_freq, cum_freq = cum_freq, surv = surv)
# 
# 
# ggplot(intertimes_distr, aes(x = intertimes, y = rel_freq)) +
#   geom_col() + ylab("density") + 
#   xlab("intertimes")
# 
# 
# ggplot(intertimes_distr, aes(x = intertimes, y = cum_freq)) +
#   geom_point(shape = 1) + 
#   geom_line(linewidth = 0.2) + ylab("ecdf") + 
#   xlab("intertimes")
# 
# 
# 
# ggplot(intertimes_distr, aes(x = log(intertimes), y = log(surv))) +
#   geom_point(shape = 1) + 
#   geom_line(linewidth = 0.2) +
#   ylab("log(survival function)") +
#   xlab("log(intertimes)") +
#   geom_smooth(method = "lm", se = FALSE, linewidth = 0.3, color = "red")
# 
# 
# ggplot(war, aes(x = days_since_start, y = intertimes)) + 
#   geom_point(pch = 1) + 
#   geom_text(
#     data = subset(war, intertimes > 150), # Filtra solo i punti con y > 200
#     aes(label = date_start),
#     hjust = -0.2, vjust = c(rep(-0.5, 4), -1.1, rep(-0.5, 2)), # Per posizionare meglio il testo
#     size = 2.3
#   )
# 
# # ANALISI INTERTEMPI
# 
# 
# x <- war$intertimes[-1]
# n <- length(x)
# lambda <- 1/mean(x)
# theoretical_quantiles <- qexp(ppoints(n), rate = lambda)
# 
# # Sort the observed data
# sorted_data <- sort(x)
# 
# # Create the QQ plot
# qqplot(theoretical_quantiles, sorted_data,
#        main = "QQ Plot: Exponential Fit",
#        xlab = "Theoretical Quantiles",
#        ylab = "Sample Quantiles")
# abline(0, 1, col = "red")  # Add a reference line
# 
# 
# library(MASS)
# fit <- fitdistr(x, "Negative Binomial")
# 
# # Extract the estimated parameters
# size <- fit$estimate["size"]  # Dispersion parameter
# mu <- fit$estimate["mu"]      # Mean parameter
# 
# # Generate theoretical quantiles
# n <- length(x)
# theoretical_quantiles <- qnbinom(ppoints(n), size = size, mu = mu)
# 
# # Sort the observed data
# sorted_data <- sort(x)
# 
# # Create the QQ plot
# qqplot(theoretical_quantiles, sorted_data,
#        main = "QQ Plot: Negative Binomial Fit",
#        xlab = "Theoretical Quantiles",
#        ylab = "Sample Quantiles")
# abline(0, 1, col = "red")  # Add a reference line


##### ANALISI ESPLROATIVA DA INSERIRE NEL REPORT ####

# grafico percentuale di tipo di morti per anno
war |> dplyr::select(c("id", "date_start", "deaths_a", "deaths_b", "deaths_civilians", "deaths_unknown", "year")) |>
  pivot_longer(cols = 3:6, names_to = "deaths_type", values_to = "n_deaths") |>  
  group_by(year, deaths_type) |> 
  summarise(n = sum(n_deaths))  |> 
  mutate(percentage = n / sum(n)) |> # View()
  ggplot(aes(x = year, y = percentage, fill = deaths_type)) +
  geom_area(alpha = 0.5 , size = 0.3, colour = "black") + 
  theme_bw() +
  scale_x_continuous(
    breaks = seq(1990, 2023, by = 5),  # Show labels for every 5 years
    minor_breaks = 1989:2023          # Add ticks for every year
  ) +
  scale_fill_discrete(
    labels = c("Israeliani", "Palestinesi", "Civili", "Stato sconosciuto")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate labels for readability
  labs(x = "Anno", y = "Percentuale", fill = "Morti")


# grafico side_b per adm_1
war |> 
  dplyr::select(c("id", "adm_1", "side_b")) |> 
  mutate(
    adm_1 = fct_recode(adm_1, 
                       "Distretto centrale" = "Central district",
                       "Striscia di Gaza" = "Gaza Strip",
                       "Distretto di Haifa" = "Haifa district",
                       "Distretto di Gerusalemme" = "Jerusalem district",
                       "Distretto settentrionale" = "Northern district",
                       "Distretto meridionale" = "Southern district",
                       "Distretto di Tel Aviv" = "Tel Aviv district",
                       "Cisgiordania" = "West Bank"
    ),
    side_b = fct_lump(side_b, 3, other_level = "Altro (AMB, PFLP, PFLP-GC, PNA, PRC)")
  ) |> 
  group_by(adm_1) |> 
  count(side_b) |> 
  group_by(adm_1) |> 
  mutate(percentage = n / sum(n)) |>  # Calcolo delle percentuali
  ggplot() + 
  geom_bar(aes(x = adm_1, y = percentage, fill = side_b), stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Etichette verticali
  labs(
    x = "Regione amministrativa", 
    y = "Percentuale", 
    fill = "Fazione palestinese contrapposta ad Israele"
  )








#### PLOT GEOGRAFICI####
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

df <- data.frame(x = war$longitude,
                 y = war$latitude)

# Load the world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define a bounding box for Israel and the surrounding region
# Adjust the range of latitudes and longitudes as necessary
region_bbox <- st_bbox(c(
  xmin = 33.0,  # Adjust this for more western longitude
  xmax = 37.5,  # Adjust this for more eastern longitude
  ymin = 29.4,  # Adjust this for more southern latitude
  ymax = 33.5   # Adjust this for more northern latitude
), crs = st_crs(4326))  # WGS84 coordinate reference system

# Crop the world map to the bounding box
region <- st_crop(world, region_bbox)

# Convert df to an sf object
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = 4326)

# Plot the data with the map

p1 <- ggplot() +
  geom_sf(data = region, fill = "lightgray", color = "black") +  # Region map
  geom_sf(data = df_sf, color = "red", size = 0.5, alpha = 0.4) +  # Points from df
  theme_minimal() +
  coord_sf(xlim = c(region_bbox$xmin, region_bbox$xmax),
           ylim = c(region_bbox$ymin, region_bbox$ymax),
           expand = FALSE) +
  labs(title = paste0("Conflict Events in Israel"),
       x = "Longitude",
       y = "Latitude")

p1 


w_isr <- read.csv("places_of_worship_israel.csv", sep = ",")[,-1]
# colnames(w_isr) <- c("x", "y", "id")

w_isr <- w_isr[which(w_isr$id %in% unique(war$id_nearest_pow)),]

w_isr_sf <- st_as_sf(w_isr, coords = c("Longitude", "Latitude"), crs = 4326)

p1 + geom_sf(data = w_isr_sf, size = 0.7, alpha = 0.3, pch = 3) + labs(subtitle = "Black crosses are places of worship (only nearest to war events)")


p_isr <- read.csv("political_places_israel.csv", sep = ",")[,-1]
# colnames(p_isr) <- c("x", "y", "id")

# p_isr <- p_isr[which(p_isr$id %in% unique(war$id_nearest_pow)),]

p_isr_sf <- st_as_sf(p_isr, coords = c("Longitude", "Latitude"), crs = 4326)

p1 + geom_sf(data = p_isr_sf, size = 0.7, alpha = 0.3, pch = 4) + labs(subtitle = "Black `x` are government places")



c_isr <- read.csv("military_checkpoints_israel.csv", sep = ",")[,-1]
# colnames(c_isr) <- c("Longitude", "Latitude", "id")

# c_isr <- c_isr[which(c_isr$id %in% unique(war$nearest_pow)),]

c_isr_sf <- st_as_sf(c_isr, coords = c("Longitude", "Latitude"), crs = 4326)

p1 + geom_sf(data = c_isr_sf, size = 0.7, alpha = 0.25, pch = 8) + labs(subtitle = "Black `*` are military checkpoints")



###---------------------------------------------------------


#### ANALISI ESPLORATIVA PROCESSO DI PUNTO ####
#### SPATSTAT ####
conflicts_points <- data.frame(x = war$x, y = war$y)

# Create SF object with correct CRS
conflicts_points_sf <- st_as_sf(conflicts_points, coords = c("x", "y"), 
                                crs = 32633)

#### LETTURA SHAPEFILE ISRAELE ####
library(sf)
# Leggi lo shapefile
israel_polygon <- st_read("israel_palestine_combined.shp")

# Verifica il sistema di coordinate originale
st_crs(israel_polygon)

# Trasforma il sistema di coordinate in UTM (metri)
israel_polygon_proj <- st_transform(israel_polygon, crs = 32633)
conflict_points_proj <- st_transform(conflicts_points_sf, crs = 32633)

# Riscalare in chilometri dividendo per 1000
israel_polygon_proj_km <- israel_polygon_proj
st_geometry(israel_polygon_proj_km) <- st_geometry(israel_polygon_proj) / 1000
israel_polygon_proj <- israel_polygon_proj_km
# Aggiorna il CRS dopo la riscalatura (CRS personalizzato o rimuovi il CRS)
st_crs(israel_polygon_proj_km) <- NA
st_crs(conflict_points_proj) <- NA

# Check intersections
intersected_points <- st_intersection(conflict_points_proj, israel_polygon_proj)

# Convert to ppp
# Convert SF to SP first, which can sometimes be more robust
conflicts_sp <- as(intersected_points, "Spatial")

# Create window
israel_window <- as.owin(israel_polygon_proj)

# Create ppp
conflicts_ppp <- ppp(
  x = coordinates(conflicts_sp)[,1],
  y = coordinates(conflicts_sp)[,2],
  window = israel_window
)
plot(conflicts_ppp, main = "Pattern spaziale")
axis(1); axis(2)


plot(density(conflicts_ppp), main = "")
plot(conflicts_ppp, add = TRUE, pch = ".", alpha = 0.05,
     cex = 0.01)


#### STOPP ####
#### stopp ####
library(stopp)
library(mgcv)
library(spatstat)
library(stpp)
library(colormap)
source("funzioni nuove e internals.R")
W <- israel_window
conflicts_points <- data.frame(x = war$x, y = war$y, t = war$days_since_start)
proc <- stp(conflicts_points)

mod_const <- stppm(proc, ~ 1, W = W)
gb_const <- globaldiag(proc, mod_const$l) 

class(proc)
stopp:::plot.stp(proc)

par(mfrow = c(1, 3))
# primo grafico
plot3D::scatter3D(proc$df$x, proc$df$y, proc$df$t, theta = -45, phi = 20, 
                  pch = 20, cex = 0.5, ticktype = "detailed", col = "black", xlab = "x", 
                  ylab = "y", zlab = "t", main = c("Point pattern spazio-temporale"))

# secondo grafico
plot(conflicts_ppp, main = "Point pattern spaziale", xlab = "x", ylab = "y")
axis(1); axis(2)
mtext("x", side = 1, line = 3, cex = 0.8)
mtext("y", side = 2, line = 3, cex = 0.8)
# terzo grafico
barplot(table(proc$df$t), main = "Point pattern temporale", 
        xlab = "t", width = 0.3)
par(mfrow = c(1, 1))

# rinominazione main
plot.globaldiag <- function (x, samescale = TRUE, ...) 
{
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  lims <- if (samescale) {
    range(c(as.numeric(x$est), as.numeric(x$theo), as.numeric(x$diffK)))
  }
  else {
    NULL
  }
  par(mfrow = c(1, 3))
  fields::image.plot(x$dist, x$times, x$est, main = "Funzione K stimata", 
                     xlab = "r", ylab = "h", col = grDevices::hcl.colors(12, 
                                                                         "YlOrRd", rev = TRUE), zlim = lims, legend.mar = 12, 
                     axes = FALSE)
  axis(1, at = seq(0, 1, l = length(x$dist)), labels = round(x$dist, 
                                                             3))
  axis(2, at = seq(0, 1, l = length(x$times)), labels = round(x$times, 
                                                              3))
  box()
  fields::image.plot(x$dist, x$times, x$theo, main = "Funzione K teorica", 
                     xlab = "r", ylab = "h", col = grDevices::hcl.colors(12, 
                                                                         "YlOrRd", rev = TRUE), zlim = lims, legend.mar = 12, 
                     axes = FALSE)
  axis(1, at = seq(0, 1, l = length(x$dist)), labels = round(x$dist, 
                                                             3))
  axis(2, at = seq(0, 1, l = length(x$times)), labels = round(x$times, 
                                                              3))
  box()
  fields::image.plot(x$dist, x$times, x$diffK, main = "Differenza", 
                     xlab = "r", ylab = "h", col = grDevices::hcl.colors(12, 
                                                                         "YlOrRd", rev = TRUE), zlim = lims, legend.mar = 15, 
                     axes = FALSE)
  axis(1, at = seq(0, 1, l = length(x$dist)), labels = round(x$dist, 
                                                             3))
  axis(2, at = seq(0, 1, l = length(x$times)), labels = round(x$times, 
                                                              3))
  box()
}

plot.globaldiag(gb_const)





