rm(list = ls())
if(!require(rvest)) install.packages("rvest")
if(!require(xml2)) install.packages("xml2")
library(stringr)
library(dplyr)

year <- 1989:2023
month <- c("January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December")

i <- 1
j <- 1
k <- 1
calendar <- vector("list", length = length(year)*length(month))

for(i in 1:length(year)){
  for(j in 1:length(month)){
    
    # URL del sito
    url <- paste0("https://www.islamicfinder.org/islamic-calendar/", year[i],"/", month[j], "/?type=Gregorian")
    
    # Leggi la pagina HTML
    page <- read_html(url)
    
    # Estrai le celle che contengono eventi
    events <- page %>%
      html_nodes("td.event")  # Seleziona i nodi <td> con la classe "event"
    
    # Estrai le date e i nomi delle festività
    dates <- events %>%
      html_node("h4") %>%  # Cerca il nodo <h4> per il giorno
      html_text() %>%      # Estrai il testo
      as.integer()         # Converti in numerico
    
    
    # Ripulisci i testi estratti
    festivities <- events %>%
      html_node("p") %>%   # Cerca il nodo <p> per il nome della festività
      html_text() %>%      # Estrai il testo
      str_squish()         # Rimuove spazi e caratteri di controllo extra
    
    
    
    # Combina le informazioni in un data frame
    calendar_list <- list(
      Date = paste0(paste0(year[i], "-", if(j < 10) paste0("0", j) else paste0(j), "-"), sprintf("%02d", dates)),  # Combina l'anno, il mese e il giorno
      Festivity = festivities
    )
    
    calendar[[k]] <- calendar_list
    k <- k + 1
    print(c(year[i], month[j]))
    print(calendar_list)
    
  }
}


calendar_1 <- lapply(calendar, function(x) {
  dates <- unlist(x[grep("Date", names(x))])
  festivities <- unlist(x[grep("Festivity", names(x))])
  cbind(dates, festivities)})

non_ok <- which(unlist(lapply(calendar_1, ncol)) == 1)

isl_cal <- as.data.frame(do.call(rbind, calendar_1[-non_ok]))
rownames(isl_cal) <- NULL
colnames(isl_cal)

write.csv(isl_cal, file = "isl_cal1.csv", row.names = F)
