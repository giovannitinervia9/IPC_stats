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

war <- war |> filter(year == 2023)
date_start <- war$date_start
date_start <- as.Date(date_start, format = "%Y-%m-%d")
days_since_start <- as.numeric(date_start - min(date_start))
war$days_since_start <- days_since_start


#### SPATSTAT ####
conflicts_points <- data.frame(x = war$x, y = war$y)

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

# # Create ppp
# conflicts_ppp <- ppp(
#   x = coordinates(conflicts_sp)[,1],
#   y = coordinates(conflicts_sp)[,2],
#   window = israel_window
# )
# plot(conflicts_ppp)
# axis(1); axis(2)
# 
# mod <- ppm(conflicts_ppp, ~ poly(x, 2) + poly(y, 2))
# mod
# plot(predict(mod))
# plot(conflicts_ppp, add = T, pch = ".", cex = 1.5, alpha = 0.05, cols = "grey")
# plot(sqrt(density(conflicts_ppp)))
# plot(log(density(conflicts_ppp)))

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



mod_cov <- stppm(proc, ~ (x + y + t)^2 + I(x^2) + I(y^2) + I(t^2) + dist_nearest_pow +
                dist_nearest_gov + dist_nearest_chp + 
                days_from_nearest_heb_cal + days_from_nearest_isl_cal,
              covs = covs, spatial.cov = T, W = W)
# plot.stppm(mod_cov, W = W, do.points = F, scaler = "var")

g_mod_cov <- globaldiag(proc, mod_cov$l)
plot3d.globaldiag(g_mod_cov)

summary(mod_cov$mod_global)



mod_non_cov <- stppm(proc, ~ (x + y + t)^2 + I(x^2) + I(y^2) + I(t^2), W = W)
g_mod_non_cov <- globaldiag(proc, mod_non_cov$l)
plot3d.globaldiag(g_mod_non_cov)
summary(mod_cov$mod_global)



library(ggeffects)
predict_response(mod_cov$mod_global, terms = "x") |> plot()
predict_response(mod_cov$mod_global, terms = "y") |> plot()
predict_response(mod_cov$mod_global, terms = "t") |> plot()

plot.stppm(x = mod_cov, W = W, scaler = "sd", do.points = F)

gb_mod_cov <- globaldiag(proc, mod_cov$l)
library(ggeffects)
dev.off()
predict_response(mod_cov$mod_global, terms = "dist_nearest_pow") |> plot()
predict_response(mod_cov$mod_global, terms = "dist_nearest_gov") |> plot()
predict_response(mod_cov$mod_global, terms = "dist_nearest_chp") |> plot()
predict_response(mod_cov$mod_global, terms = "days_from_nearest_heb_cal") |> plot()
predict_response(mod_cov$mod_global, terms = "days_from_nearest_isl_cal") |> plot()

#### LOG GAUSSIAN COX
seed <- rnbinom(1, mu = 10000, size = 0.05) + rpois(1, 100)
seed
modlgc <- stlgcppm(X = proc, formula = ~ (x + y + t)^2 + I(x^2) + I(y^2) + I(t^2), W = W,
                 cov = "separable", seed = seed)

#### analisi stime dei parametri di covarianza
library(parallel)
# B <- 1000
# seed <- sample(1:1000000, B, F)
# system.time({
#   res <- mclapply(seq_len(B), mc.cores = 16, function(i){
#     tryCatch(expr = {
#       modlgc <- stlgcppm(X = proc, formula = ~ (x + y + t)^2 + I(x^2) + I(y^2) + I(t^2), W = W,
#                          cov = "separable", seed = seed[i], max_vals = c(20, 20, 364))
#       c(modlgc$CovCoefs, seed[i])
#     }, error = function(e) conditionMessage(e))
#     
#   })
# })
# 
# res <- do.call(rbind, res)
# res <- res[order(res[, 3]), ]
# colnames(res) <- c(colnames(res)[1:3], "seed")
# res <- as.data.frame(res)
# head(res)
# 
# res[which(round(res$beta, 3) >= median(res$beta))[1], ]

modlgc <- stlgcppm(X = proc, formula = ~ (x + y + t)^2 + I(x^2) + I(y^2) + I(t^2), W = W,
                   cov = "separable", seed = 768344, max_vals = c(20, 20, 364))


####
summary(modlgc$mod_global)
# plot.stppm(modlgc, W = W, scaler = "sd", do.points = F)
g_modlgc <- globaldiag(proc, modlgc$l)

plot3d.globaldiag(g_modlgc)
plot3d.globaldiag(gb_mod_cov)
plot.globaldiag(g_modlgc)

plot.lgcpcov <- function(x, r = seq(0, 25, l = 30), h = seq(0, 3000, l = 30)){
  
  theta <- x$CovCoefs
  
  grid_rh <- expand.grid(r = r, h = h)
  grid_rh$`C(r, h)` <- theta[1]^2*exp(-grid_rh$r/theta[2])*exp(-grid_rh$h/theta[3])
  
  palette_custom <- grDevices::hcl.colors(1000, "YlOrRd", rev = TRUE)
  wireframe(
    `C(r, h)` ~ r * h, 
    data = grid_rh,
    drape = TRUE,
    scales = list(
      arrows = FALSE,
      cex = 0.5 # Riduce la dimensione delle etichette degli assi
    ),
    col.regions = palette_custom,
    colorkey = F,
    screen = list(z = -60, x = -70),
    main = "",
    par.settings = list(
      axis.text = list(cex = 0.7),  # Riduce il font delle etichette
      par.xlab.text = list(cex = 0.65),  # Riduce il font del nome dell'asse X
      par.ylab.text = list(cex = 0.65),  # Riduce il font del nome dell'asse Y
      par.zlab.text = list(cex = 0.65)   # Riduce il font del nome dell'asse Z
    )
  ) 
}

plot.lgcpcov(modlgc)


#glm() = y = b0 + b1*x1 + ... + bk*xk
#gam() = y = b0 + b1*f(x1)

mod3 <- stppm(proc, ~ s(x) + s(y) + s(t) + dist_nearest_pow +
        dist_nearest_gov + dist_nearest_chp + 
        days_from_nearest_heb_cal + days_from_nearest_isl_cal,
      covs = covs, spatial.cov = T, W = W)
gb_mod3 <- globaldiag(proc, mod3$l)
plot.stppm(mod3, W = W, scaler = "sd")
mod3
summary(mod3$mod_global)
plot3d.globaldiag(gb_mod3)
plot(war$x, mod3$y_resp)

# dist_nearest_chp days_from_nearest_heb_cal days_from_nearest_isl_cal
library(ggeffects)
predict_response(mod3$mod_global, terms = "x") |> plot()
predict_response(mod3$mod_global, terms = "y") |> plot()
predict_response(mod3$mod_global, terms = "t") |> plot()



mod_const <- stppm(proc, ~ 1, W = W)
gb_mod_const <- globaldiag(proc, mod_const$l)
plot3d.globaldiag(gb_mod_const)


mod_const_lgc <- stlgcppm(X = proc, formula = ~ 1, W = W,
                   cov = "separable")

gb_mod_const_lgc <- globaldiag(proc, mod_const_lgc$l)
plot3d.globaldiag(gb_mod_const_lgc)
