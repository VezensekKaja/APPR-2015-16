# Funkcija, ki uvozi podatke iz datoteke place_obcine.csv
uvozi.place_obcine<- function() {
  return(read.csv2("podatki/place_obcine.csv", sep = ";", as.is = TRUE,
                      skip=6, header=FALSE, na.strings = "-",
                      col.names = c("", "kraj", "mesec","bruto", "neto"),
                      fileEncoding = "windows-1250"))
}

uredi <- function(tabela, x, y, z, max = nrow(tabela)) {
  s <- seq(x, max, z+1)
  tabela[t(matrix(x:max, ncol=length(s))), y] <- tabela[s, y]
  tabela <- tabela[-s,]
  return(tabela)
}

# Zapišimo podatke v razpredelnico place.
place <- uvozi.place_obcine()
place <- place[,-1]

place <- uredi(place, 1, 1, 18)

link <- "http://www.saop.si/poslovne-informacije/podatki-za-obracun-in-opomniki/povprecne-in-minimalne-place/"
stran <- html_session(link) %>% read_html(encoding="UTF-8")
tabela <- stran %>% html_nodes(xpath="//table[1]") %>% .[[1]] %>% html_table()
Encoding(names(tabela)) <- "UTF-8"

grafpike <- ggplot(filter(place, kraj == "Ljubljana"), aes(x=mesec, y=bruto)) + geom_point()
grafcrte <- ggplot(filter(place, kraj %in% c("Koper/Capodistria", "Ljubljana", "Maribor")), aes(x=mesec, y=bruto, group = kraj, color = kraj)) + geom_line()