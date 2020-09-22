library(dplyr)

berufswahl <- read.csv("https://raw.githubusercontent.com/statistikZH/FFG_gender_berufswahl/master/gewaehlte-berufslehren-nach-haeufigkeit.csv", 
                       encoding = "UTF-8")

## Cleaning unneccassary strings
berufswahl$Gewählter.Beruf <- stringr::str_remove(berufswahl$Gewählter.Beruf, " Erweiterte Grundbildung")
berufswahl$Gewählter.Beruf <- stringr::str_remove(berufswahl$Gewählter.Beruf, " Basis-Grundbildung")
berufswahl$Gewählter.Beruf <- gsub("\\s*\\([^\\)]+\\)","",as.character(berufswahl$Gewählter.Beruf))
berufswahl$Gewählter.Beruf <- stringr::str_remove(berufswahl$Gewählter.Beruf, " EFZ")
berufswahl$Gewählter.Beruf <- stringr::str_remove(berufswahl$Gewählter.Beruf, " EBA")

total_frauen <- berufswahl[1, 5]
total_männer <- berufswahl[1, 6]

berufswahl_agg <- berufswahl %>%
  group_by(Gewählter.Beruf) %>%
  summarise(Total_Beruf = sum(Total),
            Frauen = sum(Frauen),
            Männer = sum(Männer)) %>%
  ungroup() %>%
  rename("Beruf" = Gewählter.Beruf) %>%
  filter(Beruf != "Total") %>%
  tidyr::pivot_longer(cols = c(Frauen, Männer), names_to = "Geschlecht", values_to = "Anzahl") %>%
  mutate(Total_Geschlecht = case_when(Geschlecht == "Frauen" ~ total_frauen,
                                      TRUE ~ total_männer),
      Anteil = round((Anzahl/Total_Geschlecht)*100, 1))

write.csv(berufswahl_agg, "gewaehlte-berufslehren-nach-haeufigkeit_aggregiert.csv", fileEncoding = "UTF-8")
