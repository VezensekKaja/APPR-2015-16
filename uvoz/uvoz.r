library(dplyr)

# Funkcija, ki uvozi podatke iz datoteke place_surs.csv
uvozi.place_surs<- function() {
  return(read.csv2("podatki/place_surs.csv", sep = ";", as.is = TRUE,
                      skip=6, header=FALSE,
                      col.names = c("", "kraj", "mesec","bruto", "neto"),
                      fileEncoding = "windows-1250"))
}

# Zapišimo podatke v razpredelnico place.
place <- uvozi.place_surs()

place.kraj <- lapply(seq(1, nrow(place), 130), function(x) place[x + (1:129), c(-1, -2)])
names(place.kraj) <- place[seq(1, nrow(place), 130), 2]
