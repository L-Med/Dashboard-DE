# Librerias
libs <- c('dplyr',
           'ggplot2', 'ggthemes',
           'tidyr', 'readr',
           'forcats',
           'rmarkdown', 'flexdashboard')
lapply(libs, library, character.only=T)
rm(libs)
  
# Cargar datos
data <- read_csv("Data.csv") %>%
  na.omit() %>%
  mutate(
    Date = format(
      as.Date(
        Date,
        format = "%d/%m/%Y"),
      "%d/%m/%Y"),
    Year = factor(
      format(
        as.Date(
          Date,
          format="%d/%m/%Y"),
        format = "%Y"),
      levels = c(2020, 2021, 2022)),
    Month = recode(
      factor(
        as.numeric(
          format(
            as.Date(
              Date, 
              format = "%d/%m/%Y"),
            format = "%m"))),
      "1" = "Ene",
      "2" = "Feb",
      "3" = "Mar",
      "4" = "Abr",
      "5" = "May",
      "6" = "Jun",
      "7" = "Jul",
      "8" = "Ago",
      "9" = "Sep",
      "10" = "Oct",
      "11" = "Nov",
      "12" = "Dic"),
    Day = recode(
      factor(
        as.numeric(
          format(
            as.Date(
              Date,
              format = "%d/%m/%Y"),
            format = "%w"))),
      "1" = "Lun",
      "2" = "Mar",
      "3" = "Mie",
      "4" = "Jue",
      "5" = "Vie",
      "6" = "Sab",
      "0" = "Dom")
    ) %>%
  select(Year, Month, Day, Listen, Read, Make, Review, Sentences)

# Valores Fijos
Immersion = c()
Anki <- c()

for (i in levels(data$Year)) {
  Immersion <- append(Immersion,
         round(
           sum(data %>%
                 filter(Year == as.numeric(i)) %>%
                 select(Read, Listen))/60,
           digits = 0))
}

for (i in levels(data$Year)) {
  Anki <- append(Anki,
                      round(
                        sum(data %>%
                              filter(Year == as.numeric(i)) %>%
                              select(Sentences)),
                        digits = 2))
}

current_Sentences <- sum(data %>%
                           select(Sentences))
current_Sentences_2022 <- sum(data %>%
                                filter(Year == 2022) %>%
                                select(Sentences))
          
# Por dias de la semana
bar_dias <- data %>%
  group_by(Day) %>%
  summarise(Time = round(mean(Listen + Read), digits = 0)) %>%
  pivot_longer(c(Time), names_to = "type", values_to = "val") %>%
  mutate(Day = fct_reorder(Day, val, sum)) %>%
  ggplot(aes(Day, val, label = val)) +
  geom_col(fill = "darkorange") +
  geom_text(vjust = 'middle',
            hjust = 1.2,
            size = 3,
            color = 'white') +
  lims(y = c(0, 60)) +
  coord_flip() +
  theme_hc() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Tiempo Medio de Contenido Diario (Min)")

# Por meses
bar_meses <- data %>%
  group_by(Month) %>%
  summarise(Time = round(mean(Listen + Read), digits = 0)) %>%
  pivot_longer(c(Time), names_to = "type", values_to = "val") %>%
  mutate(Month = fct_reorder(Month, val, sum)) %>%
  ggplot(aes(Month, val, label = val)) +
  geom_col(fill = "darkorange") +
  geom_text(vjust = 'middle',
            hjust = 1.2,
            size = 3,
            color = 'white') +
  lims(y = c(0, 90)) +
  coord_flip() +
  theme_hc() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Tiempo Medio de Contenido Diario por Mes (Min)")

# Anual por meses
for (i in levels(data$Year)) {
  assign(paste("bar_meses_", i, sep = ""),
         data %>%
           filter(Year == i) %>%
           group_by(Month) %>%
           summarise(Time = sum(Listen + Read)) %>%
           pivot_longer(c(Time), names_to = "type", values_to = "val") %>%
           ggplot(aes(Month, val, label = val)) +
           geom_col(fill = "darkorange") +
           geom_text(vjust = 1.2,
                     hjust = 'center',
                     size = 3,
                     color = 'white') +
           lims(y = c(0, 3000)) +
           scale_x_discrete(drop = F) +
           theme_hc() +
           theme(legend.position = "none") +
           labs(x = "", y = "", title = "")
         )
}

# Solo deja los gráficos
rm(current_Sentences)
rm(current_Sentences_2022)
rm(data)
rm(i)

rmarkdown::render("Dashboard.Rmd")
