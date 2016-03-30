# 4. faza: Analiza podatkov

#Izriše graf na katerem so napovedi do leta 2021 bruto plač za Ljubljano
ggplot(zdr %>% filter(kraj == "Ljubljana") %>%
         mutate(datum = paste(1, substr(mesec, 1, 3), leto) %>%
                  strptime("%d %b %Y") %>% as.Date()),
       aes(x=datum, y=bruto)) +
  xlim(as.Date("2007-01-01"), as.Date("2021-01-01")) +
  geom_line(size=0.5)+
  geom_point(size=3, fill="black")+
  ggtitle("Napoved rasti bruto plač do leta 2021")+
  geom_smooth(method = "lm",formula = y ~ x+I(x^(2))+I(log(x)), size = 1, fullrange = TRUE)

#Izpiše koeficient, prosti člen
lin <- lm(leto ~ bruto, data = zdr)

#Izpiše višino plač od leta 2015 do leta 2021
predict(lin, data.frame(leto = c(2015:2021)))



#Izriše graf na katerem so napovedi do leta 2021 bruto plač na površino za Cerklje na Gorenjskem
ggplot(zdr %>% filter(kraj == "Cerklje na Gorenjskem") %>%
         mutate(datum = paste(1, substr(mesec, 1, 3), leto) %>%
                  strptime("%d %b %Y") %>% as.Date()),
       aes(x=datum, y=bruto/površina)) +
  xlim(as.Date("2007-01-01"), as.Date("2021-01-01")) +
  geom_line(size=0.5)+
  geom_point(size=3, fill="black")+
  ggtitle("Napoved rasti razmerja bruto nad povrsino do leta 2021")+
  geom_smooth(method = "lm",formula = y ~ x+I(log(x)), size = 1, fullrange = TRUE)

#Izpiše koeficient, prosti člen
lin2 <- lm(leto ~ bruto/površino, data = zdr)

#Izpiše višino plač od leta 2015 do leta 2021
predict(lin2, data.frame(leto = c(2015:2021)))