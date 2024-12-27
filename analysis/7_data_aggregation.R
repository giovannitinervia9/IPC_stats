rm(list = ls())
data <- read.csv("IS_PAL.csv")
dim(data)
head(data)

nearest_pow <- read.csv("2_nearest_pow.csv")
head(nearest_pow)
dim(nearest_pow)

nearest_gov <- read.csv("3_nearest_gov.csv")
head(nearest_gov)
dim(nearest_gov)


nearest_chp <- read.csv("4_nearest_chp.csv")
head(nearest_chp)
dim(nearest_chp)


nearest_hebcal <- read.csv("5_nearest_hebcal.csv")
head(nearest_hebcal)
dim(nearest_hebcal)


nearest_islcal <- read.csv("6_nearest_islcal.csv")
head(nearest_islcal)
dim(nearest_islcal)

data <- data.frame(data,
           nearest_pow,
           nearest_gov,
           nearest_chp,
           nearest_hebcal,
           nearest_islcal)

write.csv(data, "war.csv", row.names = F)
