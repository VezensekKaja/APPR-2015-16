library(dplyr)

# Funkcija, ki uvozi podatke iz datoteke place_obcine.csv
uvozi.place_obcine<- function() {
  return(read.csv2("podatki/place_obcine.csv", sep = ";", as.is = TRUE,
                      skip=6, header=FALSE,
                      col.names = c("", "kraj", "mesec","bruto", "neto"),
                      fileEncoding = "windows-1250"))
}

# Zapišimo podatke v razpredelnico place.
place <- uvozi.place_obcine()

place.kraj <- lapply(seq(1, nrow(place), 17), function(x) place[x + (1:16), c(-1, -2)])
names(place.kraj) <- place[seq(1, nrow(place), 17), 2]
