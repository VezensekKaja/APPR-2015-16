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
tabela2 <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[1]] %>% html_table(dec = "," ) 
names(tabela2) <- 'kraj'
Encoding(names(tabela2)) <- "UTF-8"
Encoding(tabela2[[1]]) <- "UTF-8"


zdr <- right_join(place, data.frame(tabela2)) %>% select(-starts_with('NA..')) %>% rename(`površina` = `NA.`)
#zdruzeno <- merge(x = place, y = tabela2, by = "kraj", all.y = TRUE)
#zdruzeno1 <- zdruzeno %>% select(-starts_with('NA'))
#names(zdruzeno1) <- c("kraj" , "leto" , "mesec", "bruto", "neto" ,'površina')
#zdruzeno1[[6]]<-sub(",",".",zdruzeno1[[6]])

meseci <- c("Januar" = 1, "Julij" = 7)
ggplot(filter(place,
              kraj %in% c("Koper", "Ljubljana", "Maribor", 'Celje', 'Murska Sobota', 'Nova Gorica', 'Kranj', 'Novo Mesto')) %>%
         mutate(datum = paste(leto, meseci[mesec], 1, sep = "-") %>% as.Date()),
       aes(x=datum, y=neto, group = kraj, color = kraj)) +
  geom_line() + theme(axis.text.x = element_text(angle=60, hjust=1))


grafpike <- ggplot(filter(place, kraj == "Ljubljana"), aes(x=mesec, y=bruto)) + geom_point()
grafbruto <- ggplot(filter(place, kraj %in% c("Koper", "Ljubljana", "Maribor", 'Celje', 'Murska Sobota', 'Nova Gorica', 'Kranj', 'Novo Mesto')), aes(x=leto, y=bruto, group = kraj, color = kraj)) + geom_line() + theme(axis.text.x = element_text(angle=60, hjust=1))
grafneto <- ggplot(filter(place, kraj %in% c("Koper", "Ljubljana", "Maribor", 'Celje', 'Murska Sobota', 'Nova Gorica', 'Kranj', 'Novo Mesto')), aes(x=mesec, y=neto, group = kraj, color = kraj)) + geom_line() + theme(axis.text.x = element_text(angle=60, hjust=1))

