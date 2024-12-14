rm(list = ls())

# caricamento librerie
if(!require(tidyverse)) install.packages("tidyverse")

ged241 <- readRDS("ged241.rds")

data <- ged241 |> 
  filter(conflict_name == "Israel: Palestine" & country == "Israel") |> 
  select(id, year, side_b, adm_1, latitude, longitude, event_clarity, date_prec,
         date_start, date_end, deaths_a, deaths_b, deaths_civilians,
         deaths_unknown, best)

data <- data[order(data$date_start), ]

data$event_duration <- as.numeric((data$date_end - data$date_start)/(3600*24))

date_start <- data$date_start
date_start <- as.Date(date_start, format = "%Y-%m-%d")
days_since_start <- as.numeric(date_start - min(date_start))
data$days_since_start <- days_since_start
# plot(data$days_since_start)
data$intertimes <- c(NA, diff(data$days_since_start))
head(data)
write.csv(data, "IS_PAL.csv", row.names = F)
