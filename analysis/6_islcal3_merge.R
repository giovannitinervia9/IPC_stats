rm(list = ls())
i1 <- read.csv("isl_cal1.csv")
i2 <- read.csv("isl_cal2.csv")


intersection <- intersect(i1$dates, i2$dates)
ind1 <- which(i1$dates %in% intersection)
ind2 <- which(i2$dates %in% intersection)

i1[ind1, ]
i2[ind2, ]
unique(i1[ind1, "festivities"])
unique(i2[ind2, "festivities"])

#1st Shawwal: Festa della Rottura (del digiuno) 
# con cui si celebra la fine del digiuno ( sawm ) 
# del mese precedente di ramadan.
# -> Festival of Breaking the Fast

#1st Muharram: The Islamic New Year starts on the 1st of Muharram, 
# which is the first month in the Islamic calendar, and historically marks 
# the moment the prophet Mohammed [peace be unto him] fled from 
# Mecca to Yathrib (now called Medina) to escape religious persecution.
# -> Islamic New Year's Day

# indipendence Day -> 2023-11-15
# nel 2023 coincide con 1st Jumada Al-Awwal che non è una cosa molto rilevante


islam <- rbind(i1[-ind1, ], i2)
islam <- islam[order(islam$dates), ]

rownames(islam) <- NULL

write.csv(islam, "isl_cal3.csv", row.names = F)




#### adding variables 

## adding variable 
# days_since_nearest_isl_cal
isl_cal3 <- read.csv("isl_cal3.csv")
data <- read.csv("IS_PAL.csv")
xi <- as.Date(data$date_start, format = "%Y-%m-%d")
xj <- as.Date(isl_cal3$dates, format = "%Y-%m-%d")


library(foreach)
library(doParallel)

# Set up the parallel backend
num_cores <- round(parallel::detectCores()/2)  
cl <- makeCluster(num_cores)
registerDoParallel(cl)

days_since_nearest_isl_cal <- foreach(i = 1:nrow(data), .combine = rbind, .packages = "base") %dopar% {
  # Extract coordinates for the i-th row of 'data'
  xii <- xi[i]
  
  distance_from_i <- abs(xii - xj)
  
  # Return the index of the minimum distance
  res <- c(which.min(distance_from_i), min(distance_from_i))
  c(res, res[2]*sign(xii - xj[res[1]]))
}

# Stop the parallel backend
stopCluster(cl)

rownames(days_since_nearest_isl_cal) <- NULL
colnames(days_since_nearest_isl_cal) <- c("id_isl_cal", "days_from_nearest_isl_cal", "days_from_nearest_isl_cal_signed")


write.csv(days_since_nearest_isl_cal, "6_nearest_islcal.csv", row.names = FALSE)


