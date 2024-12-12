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

ymd(hebcal$Start.Date)

make_datetime(hebcal$Start.Date)


date_list <- lapply(strsplit(hebcal$Start.Date, "/"), as.integer)

date_list <- lapply(date_list, function(x){
  as.character(make_datetime(year = x[3], month = x[2], day = x[1]))
})

hebcal$Start.Date <- unlist(date_list)
str(hebcal)
write.csv(hebcal, "hebcal.csv", row.names = F)

