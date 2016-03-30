# 4. faza: Analiza podatkov
zdr <- zdr %>% mutate(datum = paste(1, substr(mesec, 1, 3), leto) %>%
                        strptime("%d %b %Y") %>% as.Date()) %>%
  mutate(timestamp = as.numeric(datum))

#Izriše graf na katerem so napovedi do leta 2021 bruto plač za Ljubljano
g1 <- ggplot(zdr %>% filter(kraj == "Ljubljana"),
             aes(x=datum, y=bruto)) +
  xlim(as.Date("2007-01-01"), as.Date("2021-01-01")) +
  geom_line(size=0.5)+
  geom_point(size=3, fill="black")+
  ggtitle("Napoved rasti bruto plač do leta 2021")+
  geom_smooth(method = "lm",formula = y ~ x+I(x^(2))+I(log(x)), size = 1, fullrange = TRUE)

#Izpiše koeficient, prosti člen
lin <- lm(data = zdr %>% filter(kraj == "Ljubljana"), bruto ~ timestamp+I(timestamp^2)+I(log(timestamp)))

#Izpiše višino plač od leta 2015 do leta 2021
napoved <- predict(lin, data.frame(timestamp = paste0(c(2015:2021), "-1-1") %>%
                          as.Date() %>% as.numeric()))
napoved <- setNames(napoved, c(2015:2021))


#Izriše graf na katerem so napovedi do leta 2021 bruto plač na površino za Cerklje na Gorenjskem
g2 <- ggplot(zdr %>% filter(kraj == "Cerklje na Gorenjskem"),
       aes(x=datum, y=bruto/povrsina)) +
  xlim(as.Date("2007-01-01"), as.Date("2021-01-01")) +
  geom_line(size=0.5)+
  geom_point(size=3, fill="black")+
  ggtitle("Napoved rasti razmerja bruto nad povrsino do leta 2021")+
  geom_smooth(method = "lm",formula = y ~ x+I(log(x)), size = 1, fullrange = TRUE)

#Izpiše koeficient, prosti člen
lin2 <- lm(data = zdr %>% filter(kraj == "Cerklje na Gorenjskem"), bruto/povrsina ~ timestamp+I(log(timestamp)))

#Izpiše višino razmerja bruto/površino od leta 2015 do leta 2021
napoved2 <- predict(lin2, data.frame(timestamp = paste0(c(2015:2021), "-1-1") %>%
                          as.Date() %>% as.numeric()))
napoved2 <- setNames(napoved2, c(2015:2021))
