rm(list = ls())
library(tidyverse)
first_year <- 1989
last_year <- 2023
years <- first_year:last_year

hebcal_list <- vector("list", length = length(years))


for(i in 1:length(years)){
  file_dir <- paste0("hebcal/hebcal_", years[i], "_eur.csv" )
  file <- read.csv(file_dir)
  ind <- grep(x = file$Start.Date, pattern = years[i])
  hebcal_list[[i]] <- file[ind, ]
}

hebcal <- do.call(rbind, hebcal_list) |> select(Subject, Start.Date)


date_list <- lapply(strsplit(hebcal$Start.Date, "/"), as.integer)

date_list <- lapply(date_list, function(x){
  as.character(make_datetime(year = x[3], month = x[2], day = x[1]))
})

hebcal$Start.Date <- unlist(date_list)
hebcal$hebcal_id <- 1:nrow(hebcal) 
write.csv(hebcal, "hebcal.csv", row.names = F)

## adding variable 
# days_since_nearest_heb_cel
hebcal <- read.csv("hebcal.csv")
data <- read.csv("IS_PAL.csv")
xi <- as.Date(data$date_start, format = "%Y-%m-%d")
xj <- as.Date(hebcal$Start.Date, format = "%Y-%m-%d")


library(foreach)
library(doParallel)

# Set up the parallel backend
num_cores <- round(parallel::detectCores()/2)  
cl <- makeCluster(num_cores)
registerDoParallel(cl)

days_since_nearest_heb_cal <- foreach(i = 1:nrow(data), .combine = rbind, .packages = "base") %dopar% {
  # Extract coordinates for the i-th row of 'data'
  xii <- xi[i]
  
  distance_from_i <- abs(xii - xj)
  
  # Return the index of the minimum distance
  res <- c(which.min(distance_from_i), min(distance_from_i))
  c(res, res[2]*sign(xii - xj[res[1]]))
}

# Stop the parallel backend
stopCluster(cl)

rownames(days_since_nearest_heb_cal) <- NULL
colnames(days_since_nearest_heb_cal) <- c("id_hebcal", "days_from_nearest_heb_cal", "days_from_nearest_heb_cal_signed")


write.csv(days_since_nearest_heb_cal, "5_nearest_hebcal.csv", row.names = FALSE)








