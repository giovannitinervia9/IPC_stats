rm(list = ls())

# caricamento librerie
if(!require(tidyverse)) install.packages("tidyverse")

ged241 <- readRDS("ged241.rds")

data <- ged241 |> 
  filter(conflict_name == "Israel: Palestine" & country == "Israel") |> 
  select(id, year, side_b, adm_1, longitude, latitude, event_clarity, date_prec,
         date_start, date_end, deaths_a, deaths_b, deaths_civilians,
         deaths_unknown, best)

set.seed(123)
data$longitude <- data$longitude + rnorm(nrow(data), 0, 0.01)
set.seed(124)
data$latitude <- data$latitude + rnorm(nrow(data), 0, 0.01)
data <- data[order(data$date_start, data$longitude, data$latitude), ]

data$event_duration <- as.numeric((data$date_end - data$date_start)/(3600*24))

date_start <- data$date_start
date_start <- as.Date(date_start, format = "%Y-%m-%d")
days_since_start <- as.numeric(date_start - min(date_start))
data$days_since_start <- days_since_start
# plot(data$days_since_start)
data$intertimes <- c(NA, diff(data$days_since_start))
head(data)
write.csv(data, "IS_PAL.csv", row.names = F)



# ##### removing duplicates ####
# 
# 
# library(foreach)
# library(doParallel)
# 
# # Set up the parallel backend
# n_cores <- round(detectCores()*0.75)
# cl <- makeCluster(n_cores)
# registerDoParallel(cl)
# split.row <- function(x) split(x, row(x))
# dj <- data[2:nrow(data), c("id", "date_start", "latitude", "longitude")]
# dj_list <- lapply(split.row(dj), function(x) unlist(x))
# nn <- nrow(data)
# 
# # Define the foreach loop
# list_dup <- foreach(i = 1:(nrow(data) - 1)) %dopar% {
#   di <- unlist(data[i, c("id", "date_start", "latitude", "longitude")]) # Extract row i
#   comparison <- lapply(dj_list[(i + 1):nn], function(x) if(all(x[-1] == di[-1])) x[1])
#   ind <- which(!unlist(lapply(comparison, is.null)))
#   unlist(comparison[ind])
# }
# 
# # Stop the cluster
# stopCluster(cl)
# 
# 
# names(list_dup) <- data$id[-nn]
# ind <- which(!unlist(lapply(list_dup, is.null)))
# 
# list_dup <- list_dup[ind]
# 
# id <- names(list_dup)
# new_row_list <- vector("list", length(id))
# 
# for(i in 1:length(id)){
#   id_dup <- as.character(list_dup[[i]])
#   x <- data[data$id == id[i], ]
#   y <- data[data$id %in% id_dup, ]
#   new_row <- data[nn + 1, ]
#   new_row$id <- paste0(c(x$id, y$id), collapse = "_")
#   new_row$year <- x$year
#   new_row$side_b <- if(all(y$side_b == x$side_b)) x$side_b else paste0(c(x$side_b, y$side_b), collapse = "_")
#   new_row$adm_1 <- x$adm_1
#   new_row$latitude <- x$latitude
#   new_row$longitude <- x$longitude
#   new_row$event_clarity <- if(all(y$event_clarity == x$event_clarity)) x$event_clarity else paste0(c(x$event_clarity, y$event_clarity), collapse = "_")
#   new_row$date_prec <- if(all(y$date_prec == x$date_prec)) x$date_prec else paste0(c(x$date_prec, y$date_prec), collapse = "_")
#   new_row$date_start <- x$date_start
#   new_row$date_end <- if(all(y$date_end == x$date_end)) x$date_end else paste0(c(x$date_end, y$date_end), collapse = "_")
#   new_row$deaths_a <- sum(c(x$deaths_a, y$deaths_a))
#   new_row$deaths_b <- sum(c(x$deaths_b, y$deaths_b))
#   new_row$deaths_civilians <- sum(c(x$deaths_civilians, y$deaths_civilians))
#   new_row$deaths_unknown <- sum(c(x$deaths_unknown, y$deaths_unknown))
#   new_row$best <- sum(c(x$best, y$best))
#   new_row$event_duration <- if(all(y$event_duration == x$event_duration)) x$event_duration else paste0(c(x$event_duration, y$event_duration), collapse = "_")
#   new_row$days_since_start <- x$days_since_start
#   new_row$intertimes <- x$intertimes
#   new_row_list[[i]] <- new_row
#   print(i)
# }
# 
# new_rows <- do.call(rbind, new_row_list)
# new_rows <- new_rows[order(new_rows$date_start, new_rows$latitude, new_rows$longitude), ]
# View(new_rows)
# 
# unlist(new_rows[10:11, c("latitude", "longitude")])
# 
# plot(data$latitude, data$latitude + rnorm(nrow(data), 0, 0.01))
# plot(data$longitude, data$longitude + rnorm(nrow(data), 0, 0.01))
