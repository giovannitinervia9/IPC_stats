rm(list = ls())
library(tidyverse)
library(rvest)


first_year <- 1989
last_year <- 2023
years <- first_year:last_year

islam_list <- vector("list", length(years))
for (i in 1:length(years)) {
  file_path <- paste0("islcal2_html/Public holidays in Gaza, Palestine for ", years[i]," - Holiday API.html")
  
  webpage <- read_html(file_path)
  table <- webpage |>
    html_node("table#holidays") |>
    html_table(fill = TRUE)
  
  if(is.null(table$Notes)) table$Notes <- NA
  table$Year <- years[i]
  islam_list[[i]] <- table
}


# Unisci tutti i data frame in un unico data frame
islam <- do.call(rbind, islam_list)

# Seleziona solo le colonne desiderate
islam <- islam[, c("Date", "Name", "Year")]


#pulizia del dataset

islam <- islam |> 
  filter(!Name %in% c("Christmas Day", "Christmas Eve", "December Solstice",
                      "June Solstice", "March Equinox", "Mother's Day",
                      "New Year's Day", "New Year's Eve", "September Equinox"))


islam <- islam %>%
  mutate(
    # Remove "th", "st", "nd", "rd" from dates
    CleanDate = gsub("(\\d+)(th|st|nd|rd)", "\\1", Date),
    # Combine Year and cleaned Date, then parse into Date format
    date = as.Date(paste(CleanDate, Year), format = "%B %d %Y")
  ) %>%
  select(-CleanDate)  # Remove the intermediate column if not needed

islam <- islam[, c("date", "Name")]
colnames(islam) <- c("dates", "festivities")


write.csv(islam, file = "isl_cal2.csv", row.names = F)
