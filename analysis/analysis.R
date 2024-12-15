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

#### ANALISI DESCRITTIVE ####

hist(war$event_duration, main = "Distribuzione della durata degli eventi", xlab = "durata degli eventi")
hist(war$event_duration[war$event_duration!=0], main = "Distribuzione della durata degli eventi 
     (tolti quelli con durata 0)", xlab = "durata degli eventi")



addmargins(table(war$event_clarity[war$event_duration!=0],
                 war$event_duration[war$event_duration!=0]))
#89 eventi su 4181 non si concludono nello stesso giorno, 52 degli 89 eventi si 
#sono conclusi il giorno successivo a quello di inizio



#distribuzione di event_clarity condizionatamente a date_prec
ggplot(data = war) + 
  geom_bar(mapping = aes(x = event_clarity, fill = date_prec), position = "fill")






#morti per anno
fatalities <- war |> 
  group_by(year) |> 
  summarise(fatalities_per_year = sum(best),
            civilian_deaths_per_year = sum(deaths_civilians))

ggplot(fatalities, aes(x = year, y = fatalities_per_year)) +
  geom_line(color = "black", linewidth = 1) +  
  geom_point(color = "red", size = 2) + # Punti rossi per enfatizzare i dati
  labs(title = "Serie Storica dei morti per Anno",
       x = "Anno",
       y = "Fatalità") +
  theme_minimal()




#densità di event_duration condizionata a event_clarity

filtered_war <- war |> 
  filter(event_duration!=0)
#distribuzione di event_duration condizionata a event_clarity

ggplot(filtered_war, aes(x = event_duration, fill = event_clarity)) +
  geom_bar(alpha = 0.7) +
  facet_wrap(~ event_clarity)



intertimes_table <- table(war$intertimes[-1])
intertimes <- as.integer(names(intertimes_table))
n_total <- sum(intertimes_table)
abs_freq <- as.vector(intertimes_table)
rel_freq <- abs_freq/n_total
cum_freq <- cumsum(rel_freq)
surv <- (1 - cum_freq) + rel_freq
intertimes_distr <- data.frame(intertimes = intertimes, abs_freq = abs_freq, 
                               rel_freq = rel_freq, cum_freq = cum_freq, surv = surv)


ggplot(intertimes_distr, aes(x = intertimes, y = rel_freq)) +
  geom_col() + ylab("density") + 
  xlab("intertimes")


ggplot(intertimes_distr, aes(x = intertimes, y = cum_freq)) +
  geom_point(shape = 1) + 
  geom_line(linewidth = 0.2) + ylab("ecdf") + 
  xlab("intertimes")



ggplot(intertimes_distr, aes(x = log(intertimes), y = log(surv))) +
  geom_point(shape = 1) + 
  geom_line(linewidth = 0.2) +
  ylab("log(survival function)") +
  xlab("log(intertimes)") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.3, color = "red")


ggplot(war, aes(x = days_since_start, y = intertimes)) + 
  geom_point(pch = 1) + 
  geom_text(
    data = subset(war, intertimes > 150), # Filtra solo i punti con y > 200
    aes(label = date_start),
    hjust = -0.2, vjust = c(rep(-0.5, 4), -1.1, rep(-0.5, 2)), # Per posizionare meglio il testo
    size = 2.3
  )

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


#### PROCESSO DI PUNTO ####


# PLOT #

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
colnames(w_isr) <- c("x", "y", "id")

w_isr <- w_isr[which(w_isr$id %in% unique(war$id_nearest_pow)),]

w_isr_sf <- st_as_sf(w_isr, coords = c("x", "y"), crs = 4326)

p1 + geom_sf(data = w_isr_sf, size = 0.7, alpha = 0.3, pch = 3) + labs(subtitle = "Black crosses are places of worship (only nearest to war events)")


p_isr <- read.csv("political_places_israel.csv", sep = ",")[,-1]
colnames(p_isr) <- c("x", "y", "id")

# p_isr <- p_isr[which(p_isr$id %in% unique(war$id_nearest_pow)),]

p_isr_sf <- st_as_sf(p_isr, coords = c("x", "y"), crs = 4326)

p1 + geom_sf(data = p_isr_sf, size = 0.7, alpha = 0.3, pch = 4) + labs(subtitle = "Black `x` are government places")



c_isr <- read.csv("military_checkpoints_israel.csv", sep = ",")[,-1]
colnames(c_isr) <- c("x", "y", "id")

# c_isr <- c_isr[which(c_isr$id %in% unique(war$nearest_pow)),]

c_isr_sf <- st_as_sf(c_isr, coords = c("x", "y"), crs = 4326)

p1 + geom_sf(data = c_isr_sf, size = 0.7, alpha = 0.25, pch = 8) + labs(subtitle = "Black `*` are military checkpoints")



#### SPATSTAT ####


conflicts_points <- data.frame(x = war$longitude, y = war$latitude)

# Create SF object with correct CRS
conflicts_points_sf <- st_as_sf(conflicts_points, coords = c("x", "y"), crs = 4326)

# Read shapefile
israel_polygon <- st_read("israel_palestine_combined.shp")

# Ensure both are in the same CRS before transformation
# Check current CRS
st_crs(conflicts_points_sf)
st_crs(israel_polygon)

# Optional: Ensure both are in WGS84 if they aren't already
conflicts_points_sf <- st_transform(conflicts_points_sf, crs = st_crs(israel_polygon))

# Project to a suitable UTM zone
israel_polygon_proj <- st_transform(israel_polygon, crs = 32633)
conflicts_points_proj <- st_transform(conflicts_points_sf, crs = 32633)

# Check intersections
intersected_points <- st_intersection(conflicts_points_proj, israel_polygon_proj)

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
plot(conflicts_ppp)


mod <- ppm(conflicts_ppp, ~ poly(x, 2) + poly(y, 2))
summary(mod)
plot(predict(mod))
plot(conflicts_ppp, add = T, pch = ".", cex = 1.5, alpha = 0.1, cols = "grey")
plot(sqrt(density(conflicts_ppp)))
plot(log(density(conflicts_ppp)))

#### stopp ####
library(stopp)
conflicts_points <- data.frame(x = war$longitude, y = war$latitude, t = war$days_since_start)
proc <- stp(conflicts_points)
mod <- stppm(proc, ~ poly(x, 2) + poly(y, 2) + poly(t, 2))
summary(mod$mod_global)


# models with covariates
covariates <- c("dist_nearest_pow", "dist_nearest_gov", "dist_nearest_chp",
                "days_from_nearest_heb_cal", "days_from_nearest_isl_cal")

if(file.exists("covs.rds")){
  covs <- readRDS("covs.rds")
} else {
  
  df_cov <- war[, which(colnames(war) %in% covariates)]
  
  df_list <- list(cov1 = cbind(conflicts_points, df_cov[, 1]),
                  cov2 = cbind(conflicts_points, df_cov[, 2]),
                  cov3 = cbind(conflicts_points, df_cov[, 3]),
                  cov4 = cbind(conflicts_points, df_cov[, 4]),
                  cov5 = cbind(conflicts_points, df_cov[, 5]))
  
  
  library(foreach)
  library(doParallel)
  
  # Set up the parallel backend
  num_cores <- min(length(covariates), round(parallel::detectCores()/2) + 1)
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  system.time({
    covs <- foreach(i = 1:length(covariates), .packages = "stopp") %dopar% {
      stcov(df_list[[i]], names = covariates[i])
    }
  })
  
  names(covs) <- covariates
  
  stopCluster(cl)
  
  saveRDS(covs, file = "covs.rds")
  
}



mod2 <- stppm(proc, ~ poly(x, 2) + poly(y, 2) + poly(t, 2) + dist_nearest_pow +
                dist_nearest_gov + dist_nearest_chp + 
                days_from_nearest_heb_cal + days_from_nearest_isl_cal,
              covs = covs, spatial.cov = T)



summary(mod2$mod_global)
plot(mod2)

# resmod2 <- localdiag(proc, mod2$l)

