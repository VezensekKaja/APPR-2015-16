# 4. faza: Analiza podatkov

#Izriše graf na katerem so napovedi do leta 2026 bruto plač
ggplot(data=zdr[['bruto']], aes(x=datum, y=bruto))+ xlim(2007, 2026) +
  geom_line(size=0.5)+
  geom_point(size=3, fill="black")+
  ggtitle("Napoved rasti bruto plač do leta 2026")+
  geom_smooth(method = "lm",formula = y ~ x+I(x^2)+I(x^3),
              size = 1, fullrange = TRUE)

