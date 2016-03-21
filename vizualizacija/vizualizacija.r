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

## Zemljevid z barvami za neto plače
placepoobcinah$kraj <- gsub("-", " - ", placepoobcinah$OB_UIME)
placepoobcinah$kraj[placepoobcinah$kraj == "Kanal"] <- "Kanal ob Soči"
placepoobcinah$kraj[placepoobcinah$kraj == "Loški Potok"] <- "Loški potok"
placepoobcinah$kraj[placepoobcinah$kraj == "Sveti Andraž v Slov. goricah"] <- "Sveti Andraž v Slovenskih goricah"

placepoobcinah$kraj <- factor(placepoobcinah$kraj)
zdr$kraj <- factor(zdr$kraj)
ob <- pretvori.zemljevid(placepoobcinah)

zem <- ggplot() + geom_polygon(data = zdr %>%
                                 filter(leto == 2015, mesec == "Julij") %>%
                                 right_join(ob), aes(x=long, y=lat, group=group, fill=neto), color = "grey") +
  scale_fill_gradient(low="#002b29", high="#00fff3") +
  guides(fill = guide_colorbar(title = "neto"))
print(zem)

zem2 <- zem +geom_point(data = zdr %>%
                     filter(leto == 2015, mesec == "Julij", neto > 1000) %>%
                     inner_join(placepoobcinah@data), aes(x = Y_C, y = X_C))
print(zem2)

zem3 <- zem2 + geom_text(data = zdr %>% filter(leto == 2015, mesec == "Julij", neto > 1000)
                         %>% inner_join(placepoobcinah@data),
                         aes(x = Y_C, y = X_C, label = kraj),
                         size = 3, vjust = 2)
print(zem3)

## Zemljevid za bruto plače glede na površino po občinah

bnppoobcinah <- uvozi.zemljevid("http://e-prostor.gov.si/fileadmin/BREZPLACNI_POD/RPE/OB.zip",
                                  "OB/OB", encoding = "Windows-1250")
## Zemljevid z barvami
bnppoobcinah$kraj <- gsub("-", " - ", bnppoobcinah$OB_UIME)
bnppoobcinah$kraj[bnppoobcinah$kraj == "Kanal"] <- "Kanal ob Soči"
bnppoobcinah$kraj[bnppoobcinah$kraj == "Loški Potok"] <- "Loški potok"
bnppoobcinah$kraj[bnppoobcinah$kraj == "Sveti Andraž v Slov. goricah"] <- "Sveti Andraž v Slovenskih goricah"

bnppoobcinah$kraj <- factor(bnppoobcinah$kraj)
zdr$kraj <- factor(zdr$kraj)
ob <- pretvori.zemljevid(bnppoobcinah)

zembnp <- ggplot() + geom_polygon(data = zdr %>%
                                 filter(leto == 2015, mesec == "Julij") %>%
                                 right_join(ob), aes(x=long, y=lat, group=group, fill=bruto/površina), color = "grey") +
  scale_fill_gradient(low="#FFFF99", high="#000066") +
  guides(fill = guide_colorbar(title = "bruto/površina"))
print(zembnp)

zembnp2 <- zembnp +geom_point(data = zdr %>%
                          filter(leto == 2015, mesec == "Julij", bruto/površina > 50) %>%
                          inner_join(bnppoobcinah@data), aes(x = Y_C, y = X_C))
print(zembnp2)


zembnp3 <- zembnp2 + geom_text(data = zdr %>% filter(leto == 2015, mesec == "Julij", bruto/površina > 50)
                         %>% inner_join(bnppoobcinah@data),
                         aes(x = Y_C, y = X_C, label = kraj),
                         size = 3, vjust = 2)
print(zembnp3)

