# Funkcija, ki uvozi podatke iz datoteke place_obcine.csv
uvozi.place_obcine<- function() {
  return(read.csv("podatki/place_obcine.csv", sep = ";", as.is = TRUE,
                      skip=6, header=FALSE, na.strings = "-",
                      col.names = c("", "kraj", "leto", "mesec","bruto", "neto"),
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

link <- "https://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
stran <- html_session(link) %>% read_html(encoding="UTF-8")
tabela <- stran %>% html_nodes(xpath="//table[1]") %>% .[[2]] %>% html_table()
Encoding(names(tabela)) <- "UTF-8"

tabelazakraj <- filter(place, kraj == "Ljubljana")
grafpike <- ggplot(filter(place, kraj == "Ljubljana"), aes(x=mesec, y=bruto)) + geom_point()
grafbruto <- ggplot(filter(place, kraj %in% c("Koper/Capodistria", "Ljubljana", "Maribor", 'Celje', 'Murska Sobota', 'Nova Gorica', 'Kranj', 'Novo Mesto')), aes(x=mesec, y=bruto, group = kraj, color = kraj)) + geom_line() + theme(axis.text.x = element_text(angle=60, hjust=1))
grafneto <- ggplot(filter(place, kraj %in% c("Koper/Capodistria", "Ljubljana", "Maribor", 'Celje', 'Murska Sobota', 'Nova Gorica', 'Kranj', 'Novo Mesto')), aes(x=mesec, y=neto, group = kraj, color = kraj)) + geom_line() + theme(axis.text.x = element_text(angle=60, hjust=1))
