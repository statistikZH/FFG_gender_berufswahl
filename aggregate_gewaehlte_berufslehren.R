library(dplyr)

berufswahl <- read.csv("https://raw.githubusercontent.com/statistikZH/FFG_gender_berufswahl/master/gewaehlte-berufslehren-nach-haeufigkeit.csv", 
                       encoding = "UTF-8")

berufswahl$Gewählter.Beruf <- stringr::str_remove(berufswahl$Gewählter.Beruf, " EFZ")
berufswahl$Gewählter.Beruf <- stringr::str_remove(berufswahl$Gewählter.Beruf, " EBA")
berufswahl$Gewählter.Beruf <- stringr::str_remove(berufswahl$Gewählter.Beruf, " Erweiterte Grundbildung")
berufswahl$Gewählter.Beruf <- stringr::str_remove(berufswahl$Gewählter.Beruf, " Basis-Grundbildung")

total_frauen <- berufswahl[1, 5]
total_männer <- berufswahl[1, 6]

berufswahl_agg <- berufswahl %>%
  group_by(Gewählter.Beruf) %>%
  summarise(Total = sum(Total),
            Frauen = sum(Frauen),
            Männer = sum(Männer),
            Anteil = sum(Anteil.in..),
            Anteil_Frauen = (Frauen/total_frauen) * 100,
            Anteil_Männer = (Männer/total_männer) *100) %>%
  ungroup() %>%
  rename("Beruf" = Gewählter.Beruf)

write.csv(berufswahl_agg, "gewaehlte-berufslehren-nach-haeufigkeit_aggregiert.csv")
