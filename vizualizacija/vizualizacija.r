# 3. faza: Izdelava zemljevida

source("lib/uvozi.zemljevid.r", encoding = "UTF-8")
library(ggplot2)
library(dplyr)

pretvori.zemljevid <- function(zemljevid) {
  fo <- fortify(zemljevid)
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}

# 1. Slovenske občine

placepoobcinah <- uvozi.zemljevid("http://e-prostor.gov.si/fileadmin/BREZPLACNI_POD/RPE/OB.zip",
                          "OB/OB", encoding = "Windows-1250")

## Zemljevid z barvami za površino
zem <- ggplot() + geom_polygon(data = zdr, aes(x=long, y=lat, group=group,
                                              fill=neto),
                               color = "grey") +
  scale_fill_gradient(low="#3F7F3F", high="#00FF00") +
  guides(fill = guide_colorbar(title = "neto"))
print(zem)
