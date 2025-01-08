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

war_nf |> group_by(year) |> count() |> 
  ggplot(aes(x = year, y = n)) + 
  geom_point() + geom_line(linewidth = 0.4) + 
  theme_bw() + xlab("Anni") + ylab("Frequenze") + 
  scale_y_continuous(breaks = seq(0, 1500, by = 200)) + 
  scale_x_continuous(breaks = seq(1990, 2023, by = 5))
  

war |> group_by(year) |> count() |> arrange(-year)  
war_nf |> group_by(year) |> count() |> arrange(-year)

war_nf <- read.csv("IS_PAL_non_filtered.csv")


#### ANALISI ESPLORATIVE ADM_1, DEATHS ####
war_nf$side_b <- factor(war_nf$side_b)

war_nf$adm_1[which(is.na(war_nf$adm_1))] <- "Unknown"
war_nf$adm_1 <- factor(war_nf$adm_1, levels = c("Central district",
                                                "Gaza Strip", "Haifa district", 
                                                "Jerusalem district",
                                                "Northern district",
                                                "Southern district",
                                                "Tel Aviv district",
                                                "West Bank", "Unknown"))



war_nf$event_clarity <- factor(war_nf$event_clarity)
war_nf$date_prec <- factor(war_nf$date_prec)
colnames(war_nf)



library(dplyr)
library(tidyr)
library(ggplot2)
library(MetBrewer)
# grafico percentuale di tipo di morti per anno
war_nf |> dplyr::select(c("id", "date_start", "deaths_a", "deaths_b", "deaths_civilians", "year")) |>
  pivot_longer(cols = 3:5, names_to = "deaths_type", values_to = "n_deaths") |>  
  group_by(year, deaths_type) |> 
  summarise(n = sum(n_deaths))  |> 
  mutate(percentage = n / sum(n)) |> 
  mutate(deaths_type = case_when(
    deaths_type == "deaths_a" ~ "Israeliani (non civili)",
    deaths_type == "deaths_b" ~ "Palestinesi (non civili)",
    deaths_type == "deaths_civilians" ~ "Civili"
  )) |> 
  ggplot(aes(x = year, y = percentage, fill = deaths_type)) +
  geom_bar(stat = "identity", alpha = 0.7, size = 0.3, colour = "black", show.legend = T) +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(1990, 2023, by = 3),  # Show labels for every 5 years
    minor_breaks = 1989:2023          # Add ticks for every year
  ) +
 # scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.7,"cm")) +  # Rotate labels for readability
  labs(x = "Anno", y = "Frequenza relativa", fill = "Status dei deceduti") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))
 


# grafico side_b per adm_1
war_nf |> 
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
                       "Cisgiordania" = "West Bank",
                       "Sconosciuta" = "Unknown"
    ),
    side_b = fct_lump(side_b, 3, other_level = "Altro (AMB, PFLP, PFLP-GC, PNA, PRC)")
  ) |> 
  group_by(adm_1) |> 
  count(side_b) |> 
  group_by(adm_1) |> 
  mutate(percentage = n / sum(n)) |>  # Calcolo delle percentuali
  ggplot() + 
  geom_bar(aes(x = adm_1, y = percentage, fill = side_b), stat = "identity",
           alpha = 0.8) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.7,"cm")) +  # Etichette verticali
  labs(
    x = "Regione amministrativa", 
    y = "Frequenza relativa", 
    fill = "Fazione palestinese contrapposta ad Israele")




war_nf |> 
  select(c("adm_1", "deaths_a", "deaths_b", "deaths_civilians")) |> 
  filter(adm_1 != "Unknown") |> 
  pivot_longer(cols = 2:4, names_to = "deaths_type") |> 
  mutate(deaths_type = case_when(
    deaths_type == "deaths_a" ~ "Israeliani (non civili)",
    deaths_type == "deaths_b" ~ "Palestinesi (non civili)",
    deaths_type == "deaths_civilians" ~ "Civili"
  )) |> group_by(adm_1, deaths_type) |> mutate(n = sum(value)) |> unique()




war_nf |> 
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
                       "Cisgiordania" = "West Bank",
                       "Sconosciuta" = "Unknown"
    ),
    side_b = fct_lump(side_b, 3, other_level = "Altro (AMB, PFLP, PFLP-GC, PNA, PRC)")
  ) |> 
  group_by(adm_1) |> 
  count(side_b) |> 
  group_by(adm_1) |> 
  mutate(percentage = n / sum(n)) |>  # Calcolo delle percentuali
  ggplot() + 
  geom_bar(aes(x = adm_1, y = percentage, fill = side_b), stat = "identity",
           alpha = 0.8) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.7,"cm")) +  # Etichette verticali
  labs(
    x = "Regione amministrativa", 
    y = "Frequenza relativa", 
    fill = "Fazione palestinese contrapposta ad Israele")










#### PLOT GEOGRAFICI####
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

war <- read.csv("war.csv")

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

# luoghi di culto
w_isr <- read.csv("places_of_worship_israel.csv", sep = ",")[,-1]
w_isr <- w_isr[which(w_isr$id %in% unique(war$id_nearest_pow)),]
w_isr_sf <- st_as_sf(w_isr[, 1:2], coords = c("Longitude", "Latitude"), crs = 4326)
# checkpoint militari
c_isr <- read.csv("military_checkpoints_israel.csv", sep = ",")[,-1]
c_isr <- c_isr[which(c_isr$id %in% unique(war$id_nearest_chp)),]
c_isr_sf <- st_as_sf(c_isr[, 1:2], coords = c("Longitude", "Latitude"), crs = 4326)
# edifici governativi
p_isr <- read.csv("political_places_israel.csv", sep = ",")[,-1]
p_isr <- p_isr[which(p_isr$id %in% unique(war$id_nearest_gov)),]
p_isr_sf <- st_as_sf(p_isr[, 1:2], coords = c("Longitude", "Latitude"), crs = 4326)


# Supponiamo di avere quattro sf diversi:
# df_sf      -> Eventi
# w_isr_sf   -> Luoghi di culto (+)
# c_isr_sf   -> Checkpoint militari (*)
# p_isr_sf   -> Edifici governativi (x)

# Aggiungo una colonna "cat" che specifica la categoria
df_sf      <- df_sf    %>% mutate(cat = "Eventi")
w_isr_sf   <- w_isr_sf %>% mutate(cat = "Luoghi di culto")
c_isr_sf   <- c_isr_sf %>% mutate(cat = "Checkpoint militari")
p_isr_sf   <- p_isr_sf %>% mutate(cat = "Edifici governativi")

# Unisco tutto in un unico data frame (facoltativo, ma comodo)
all_data <- rbind(df_sf, w_isr_sf, c_isr_sf, p_isr_sf)
all_data$cat <- factor(all_data$cat, levels = c("Eventi", "Luoghi di culto",
                                                "Checkpoint militari",
                                                "Edifici governativi"))

# Definisco i valori di colore e forma per ciascun livello di cat
col_vals <- c("Eventi"                  = "red",
              "Luoghi di culto"     = "black",
              "Checkpoint militari" = "black",
              "Edifici governativi" = "black")

shape_vals <- c("Eventi"                  = 16,  # cerchio pieno
                "Luoghi di culto"     = 3,   # +
                "Checkpoint militari" = 8,   # *
                "Edifici governativi" = 4)   # x

# Elenco dei livelli in ordine
cat_levels <- c("Eventi", 
                "Luoghi di culto", 
                "Checkpoint militari", 
                "Edifici governativi")

ggplot() +
  geom_sf(data = region, fill = "lightgray", color = "black") +
  # Filtro "Eventi"
  geom_sf(data = all_data,
          aes(color = cat, shape = cat),
          alpha = 0.4, size = 1, show.legend = F) +
  # Ma nelle scale metto TUTTI i livelli
  scale_color_manual(
    name   = NULL,            # stesso "name" per color e shape
    values = col_vals,
    breaks = cat_levels
  ) +
  scale_shape_manual(
    name   = NULL,
    values = shape_vals,
    breaks = cat_levels
  ) +
  # Per avere alpha pieno in legenda
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 2))
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~cat)



####---------------------------------------------------------


#### ANALISI ESPLORATIVA PROCESSO DI PUNTO ####


#### ANALISI INTERTEMPI

intertimes_table <- table(war$intertimes[-1])
intertimes <- as.integer(names(intertimes_table))
n_total <- sum(intertimes_table)
abs_freq <- as.vector(intertimes_table)
rel_freq <- abs_freq/n_total
# cum_freq <- cumsum(rel_freq)
# surv <- (1 - cum_freq) + rel_freq
intertimes_distr <- data.frame(intertimes = intertimes, abs_freq = abs_freq,
                               rel_freq = rel_freq)

summary(war$intertimes[-1])

ggplot(intertimes_distr, aes(x = intertimes, y = rel_freq)) +
  geom_col() + ylab("Frequenze relative") +
  xlab("Intertempi") + theme_bw()


ggplot(data.frame(intertimes = war$intertimes[-1]), aes(x = intertimes)) +
  stat_ecdf(geom = "step", color = "black", linewidth = 0.3) + # Line for ECDF
  geom_point(stat = "ecdf", color = "black", shape = 1) + # Points on the ECDF
  labs(title = "",
       x = "Intertempi",
       y = "Funzione di ripartizione empirica") +
  theme_bw()

ggplot(war[-1,], aes(x = as_date(date_start), y = intertimes)) +
  geom_point(pch = 1) +
  geom_text(
    data = subset(war, intertimes > 150), # Filtra solo i punti con y > 200
    aes(label = date_start),
    hjust = -0.2, vjust = c(rep(-0.5, 4), -1.1, rep(-0.5, 2)), # Per posizionare meglio il testo
    size = 2.3
  ) + xlab("Anni") + ylab("Intertempi") +
  theme_bw()


war |> group_by(year) |> count() |> 
  ggplot(aes(x = year, y = n)) +
  geom_point() + geom_line(linewidth = 0.) + 
  xlab("Anni") + ylab("Frequenze") +
  theme_bw() + 
  scale_x_continuous(minor_breaks = 1989:2023,
                     breaks = seq(1990, 2023, by = 3)) + 
  scale_y_continuous(breaks = seq(0, 1000, by = 200)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ANALISI INTERTEMPI


x <- war$intertimes[-1]
n <- length(x)
lambda <- 1/mean(x)
theoretical_quantiles <- qexp(ppoints(n), rate = lambda)

# Sort the observed data
sorted_data <- sort(x)

# Create the QQ plot
qqplot(theoretical_quantiles, sorted_data,
       main = "QQ Plot: Exponential Fit",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red")  # Add a reference line


library(MASS)
fit <- fitdistr(x, "Negative Binomial")

# Extract the estimated parameters
size <- fit$estimate["size"]  # Dispersion parameter
mu <- fit$estimate["mu"]      # Mean parameter

# Generate theoretical quantiles
n <- length(x)
theoretical_quantiles <- qnbinom(ppoints(n), size = size, mu = mu)

# Sort the observed data
sorted_data <- sort(x)

# Create the QQ plot
qqplot(theoretical_quantiles, sorted_data,
       main = "QQ Plot: Negative Binomial Fit",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red")  # Add a reference line


















#### SPATSTAT ####
conflicts_points <- data.frame(x = war$x, y = war$y, year = war$year)

# Create SF object with correct CRS
conflicts_points_sf <- st_as_sf(conflicts_points, coords = c("x", "y"), 
                                crs = 2039)

#### LETTURA SHAPEFILE ISRAELE ####
library(sf)
# Leggi lo shapefile
israel_polygon <- st_read("israel_palestine_combined.shp")

# Verifica il sistema di coordinate originale
st_crs(israel_polygon)

# Trasforma il sistema di coordinate in UTM (metri)
israel_polygon_proj <- st_transform(israel_polygon, crs = 2039)
conflict_points_proj <- st_transform(conflicts_points_sf, crs = 2039)

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
plot(unmark(conflicts_ppp), main = "Pattern spaziale")
axis(1); axis(2)


conflicts_ppp
plot(density(conflicts_ppp), main = "Densità non parametrica")
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
war$date_start

conflicts_points <- data.frame(x = war$x, y = war$y, t = war$days_since_start)
proc <- stp(conflicts_points)


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
# plot3_data processing
plot3_data <- war |> 
  select(date_start, year) |>
  mutate(date_start = as_date(date_start)) |> 
  group_by(date_start, year) |> 
  count()

# Calculate the point with the maximum value
max_point <- plot3_data[which.max(plot3_data$n), ]

# Plot without axes
plot(plot3_data$date_start,
     plot3_data$n, 
     type = "h", 
     axes = F, 
     xlab = "Anno", 
     ylab = "",
     main = "Point pattern temporale")

# Add the x-axis with years as labels
axis(1, 
     at = as.numeric(as.Date(paste0(c(seq(1989, 2023, by = 5), 2023), "-01-01"))), 
     labels = c(seq(1989, 2023, by = 5), 2023), 
     tick = T, pos = 0, las = 2)

# Add the y-axis
axis(2,
     at = seq(0, 70, by = 10), labels = seq(0, 70, by = 10))

# Add the annotation for the maximum point
text(x = as.numeric(max_point$date_start), 
     y = max_point$n, 
     labels = as.character(max_point$date_start), 
     pos = 3,  # Position above the bar
     col = "red",  # Red color for the label
     cex = 0.8)  # Adjust label size

par(mfrow = c(1, 1))

#### FUNZIONE K ####



mod_const <- stppm(proc, ~ 1, W = W)
gb_const <- globaldiag(proc, mod_const$l) 


plot3d.globaldiag(gb_const)


plot.globaldiag(gb_const)


###### SOLO 2023 ---------------------- ##########
#### PLOT GEOGRAFICI####
rm(list = ls())
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
# dev.off()
war <- read.csv("war.csv")
war <- war |> filter(year == 2023)

date_start <- war$date_start
date_start <- as.Date(date_start, format = "%Y-%m-%d")
days_since_start <- as.numeric(date_start - min(date_start))
war$days_since_start <- days_since_start



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

# luoghi di culto
w_isr <- read.csv("places_of_worship_israel.csv", sep = ",")[,-1]
w_isr <- w_isr[which(w_isr$id %in% unique(war$id_nearest_pow)),]
w_isr_sf <- st_as_sf(w_isr[, 1:2], coords = c("Longitude", "Latitude"), crs = 4326)
# checkpoint militari
c_isr <- read.csv("military_checkpoints_israel.csv", sep = ",")[,-1]
c_isr <- c_isr[which(c_isr$id %in% unique(war$id_nearest_chp)),]
c_isr_sf <- st_as_sf(c_isr[, 1:2], coords = c("Longitude", "Latitude"), crs = 4326)
# edifici governativi
p_isr <- read.csv("political_places_israel.csv", sep = ",")[,-1]
p_isr <- p_isr[which(p_isr$id %in% unique(war$id_nearest_gov)),]
p_isr_sf <- st_as_sf(p_isr[, 1:2], coords = c("Longitude", "Latitude"), crs = 4326)


# Supponiamo di avere quattro sf diversi:
# df_sf      -> Eventi
# w_isr_sf   -> Luoghi di culto (+)
# c_isr_sf   -> Checkpoint militari (*)
# p_isr_sf   -> Edifici governativi (x)

# Aggiungo una colonna "cat" che specifica la categoria
df_sf      <- df_sf    %>% mutate(cat = "Eventi")
w_isr_sf   <- w_isr_sf %>% mutate(cat = "Luoghi di culto")
c_isr_sf   <- c_isr_sf %>% mutate(cat = "Checkpoint militari")
p_isr_sf   <- p_isr_sf %>% mutate(cat = "Edifici governativi")

# Unisco tutto in un unico data frame (facoltativo, ma comodo)
all_data <- rbind(df_sf, w_isr_sf, c_isr_sf, p_isr_sf)
all_data$cat <- factor(all_data$cat, levels = c("Eventi", "Luoghi di culto",
                                                "Checkpoint militari",
                                                "Edifici governativi"))

# Definisco i valori di colore e forma per ciascun livello di cat
col_vals <- c("Eventi"                  = "red",
              "Luoghi di culto"     = "black",
              "Checkpoint militari" = "black",
              "Edifici governativi" = "black")

shape_vals <- c("Eventi"                  = 16,  # cerchio pieno
                "Luoghi di culto"     = 3,   # +
                "Checkpoint militari" = 8,   # *
                "Edifici governativi" = 4)   # x

# Elenco dei livelli in ordine
cat_levels <- c("Eventi", 
                "Luoghi di culto", 
                "Checkpoint militari", 
                "Edifici governativi")

ggplot() +
  geom_sf(data = region, fill = "lightgray", color = "black") +
  # Filtro "Eventi"
  geom_sf(data = all_data,
          aes(color = cat, shape = cat),
          alpha = 0.4, size = 1, show.legend = F) +
  # Ma nelle scale metto TUTTI i livelli
  scale_color_manual(
    name   = NULL,            # stesso "name" per color e shape
    values = col_vals,
    breaks = cat_levels
  ) +
  scale_shape_manual(
    name   = NULL,
    values = shape_vals,
    breaks = cat_levels
  ) +
  # Per avere alpha pieno in legenda
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 2))
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~cat)



####---------------------------------------------------------


#### ANALISI ESPLORATIVA PROCESSO DI PUNTO ####


#### ANALISI INTERTEMPI

intertimes_table <- table(war$intertimes[-1])
intertimes <- as.integer(names(intertimes_table))
n_total <- sum(intertimes_table)
abs_freq <- as.vector(intertimes_table)
rel_freq <- abs_freq/n_total
# cum_freq <- cumsum(rel_freq)
# surv <- (1 - cum_freq) + rel_freq
intertimes_distr <- data.frame(intertimes = intertimes, abs_freq = abs_freq,
                               rel_freq = rel_freq)

summary(war$intertimes[-1])

ggplot(intertimes_distr, aes(x = intertimes, y = rel_freq)) +
  geom_col() + ylab("Frequenze relative") +
  xlab("Intertempi") + theme_bw()


ggplot(data.frame(intertimes = war$intertimes[-1]), aes(x = intertimes)) +
  stat_ecdf(geom = "step", color = "black", linewidth = 0.3) + # Line for ECDF
  geom_point(stat = "ecdf", color = "black", shape = 1) + # Points on the ECDF
  labs(title = "",
       x = "Intertempi",
       y = "Funzione di ripartizione empirica") +
  theme_bw()

ggplot(war[-1,], aes(x = as_date(date_start), y = intertimes)) +
  geom_point(pch = 1) +
  geom_text(
    data = subset(war, intertimes > 150), # Filtra solo i punti con y > 200
    aes(label = date_start),
    hjust = -0.2, vjust = c(rep(-0.5, 4), -1.1, rep(-0.5, 2)), # Per posizionare meglio il testo
    size = 2.3
  ) + xlab("Anni") + ylab("Intertempi") +
  theme_bw()


war |> group_by(year) |> count() |> 
  ggplot(aes(x = year, y = n)) +
  geom_point() + geom_line(linewidth = 0.) + 
  xlab("Anni") + ylab("Frequenze") +
  theme_bw() + 
  scale_x_continuous(minor_breaks = 1989:2023,
                     breaks = seq(1990, 2023, by = 3)) + 
  scale_y_continuous(breaks = seq(0, 1000, by = 200)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ANALISI INTERTEMPI


x <- war$intertimes[-1]
n <- length(x)
lambda <- 1/mean(x)
theoretical_quantiles <- qexp(ppoints(n), rate = lambda)

# Sort the observed data
sorted_data <- sort(x)

# Create the QQ plot
qqplot(theoretical_quantiles, sorted_data,
       main = "QQ Plot: Exponential Fit",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red")  # Add a reference line


library(MASS)
fit <- fitdistr(x, "Negative Binomial")

# Extract the estimated parameters
size <- fit$estimate["size"]  # Dispersion parameter
mu <- fit$estimate["mu"]      # Mean parameter

# Generate theoretical quantiles
n <- length(x)
theoretical_quantiles <- qnbinom(ppoints(n), size = size, mu = mu)

# Sort the observed data
sorted_data <- sort(x)

# Create the QQ plot
qqplot(theoretical_quantiles, sorted_data,
       main = "QQ Plot: Negative Binomial Fit",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red")  # Add a reference line


















#### SPATSTAT ####
conflicts_points <- data.frame(x = war$x, y = war$y, year = war$year)

# Create SF object with correct CRS
conflicts_points_sf <- st_as_sf(conflicts_points, coords = c("x", "y"), 
                                crs = 2039)

#### LETTURA SHAPEFILE ISRAELE ####
library(sf)
# Leggi lo shapefile
israel_polygon <- st_read("israel_palestine_combined.shp")

# Verifica il sistema di coordinate originale
st_crs(israel_polygon)

# Trasforma il sistema di coordinate in UTM (metri)
israel_polygon_proj <- st_transform(israel_polygon, crs = 2039)
conflict_points_proj <- st_transform(conflicts_points_sf, crs = 2039)

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
plot(unmark(conflicts_ppp), main = "Pattern spaziale")
axis(1); axis(2)


conflicts_ppp
plot(density(conflicts_ppp), main = "Densità non parametrica")
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
war$date_start

conflicts_points <- data.frame(x = war$x, y = war$y, t = war$days_since_start)
proc <- stp(conflicts_points)


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
# plot3_data processing
plot3_data <- war |> 
  dplyr::select(date_start) |>
  mutate(date_start = as_date(date_start)) |> 
  group_by(date_start) |> 
  count()


# Plot senza assi predefiniti
plot(plot3_data$date_start,
     plot3_data$n, 
     type = "h", 
     axes = FALSE,  # Disabilita gli assi predefiniti
     xlab = "", 
     ylab = "",
     main = "Point pattern temporale")

# Aggiungi l'asse x con etichette personalizzate in italiano
axis(1, 
     at = seq(min(plot3_data$date_start), max(plot3_data$date_start), by = "month"), 
     labels = format(seq(min(plot3_data$date_start), max(plot3_data$date_start), by = "month"), "%b %Y") %>%
       gsub("Jan", "Gen", .) %>%
       gsub("Feb", "Feb", .) %>%
       gsub("Mar", "Mar", .) %>%
       gsub("Apr", "Apr", .) %>%
       gsub("May", "Mag", .) %>%
       gsub("Jun", "Giu", .) %>%
       gsub("Jul", "Lug", .) %>%
       gsub("Aug", "Ago", .) %>%
       gsub("Sep", "Set", .) %>%
       gsub("Oct", "Ott", .) %>%
       gsub("Nov", "Nov", .) %>%
       gsub("Dec", "Dic", .), las = 2)

# Aggiungi l'asse y
axis(2)

par(mfrow = c(1, 1))

#### FUNZIONE K ####



mod_const <- stppm(proc, ~ 1, W = W)
gb_const <- globaldiag(proc, mod_const$l) 


plot3d.globaldiag(gb_const)


plot.globaldiag(gb_const)
