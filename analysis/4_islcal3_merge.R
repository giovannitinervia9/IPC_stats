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
