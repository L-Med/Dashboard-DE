# Librerias
libs <- c('dplyr',
           'ggplot2', 'ggthemes',
           'tidyr', 'readr',
           'lubridate',
           'forcats',
           'rmarkdown', 'flexdashboard')
lapply(libs, library, character.only=T)
rm(libs)

# Cargar datos
data <- read_csv("Data.csv") %>%
  na.omit() %>%
  mutate(
    Date = dmy(Date),
    Year = factor(year(Date),
                  levels=c(2020, 2021, 2022)),
    Month = factor(month(Date),
                   levels = c(1:12),
                   labels = c("Ene",
                                     "Feb",
                                     "Mar",
                                     "Abr",
                                     "May",
                                     "Jun",
                                     "Jul",
                                     "Ago",
                                     "Sep",
                                     "Oct",
                                     "Nov",
                                     "Dic")),
    Day = factor(wday(Date), 
                 levels = c(1:7),
                 labels = c("Lun",
                            "Mar",
                            "Mié",
                            "Jue",
                            "Vie",
                            "Sab",
                            "Dom"))
  ) %>%
  select(Year, Month, Day, Listen, Read, Make, Review, Sentences)

# Valores Fijos
Immersion_2020 <- round(
  sum(data %>%
        filter(Year == 2020) %>%
        mutate(Immersion = Read + Listen) %>%
        select(Immersion))/60, digits = 2)

Immersion_2021 <- round(
  sum(data %>%
        filter(Year == 2021) %>%
        mutate(Immersion = Read + Listen) %>%
        select(Immersion))/60, digits = 2)

Immersion_2022 <- round(
  sum(data %>%
        filter(Year == 2022) %>%
        mutate(Immersion = Read + Listen) %>%
        select(Immersion))/60, digits = 2)

Anki_2020 <- 229.00

Anki_2021 <- round(
  sum(data %>%
        filter(Year == 2021) %>%
        select(Sentences)) - Anki_2020, digits = 2)

Anki_2022 <- round(
  sum(data %>%
        filter(Year == 2022) %>%
        select(Sentences)), digits = 2)

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
            size = 3.2,
            color = 'white') +
  coord_flip() +
  theme_hc() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Tiempo Medio de Contenido por Día (Min)")

# Por meses
bar_meses <- data %>%
  group_by(Month) %>%
  summarise(Time = round(mean(Listen + Read)/60, digits = 2)) %>%
  pivot_longer(c(Time), names_to = "type", values_to = "val") %>%
  mutate(Month = fct_reorder(Month, val, sum)) %>%
  ggplot(aes(Month, val, label = val)) +
  geom_col(fill = "darkorange") +
  geom_text(vjust = 'middle',
            hjust = 1.2,
            size = 3.2,
            color = 'white') +
  coord_flip() +
  theme_hc() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Tiempo Medio de Contenido Diario por Mes (Horas)")
          
# Por meses y por años
bar_meses_2020 <- data %>%
  filter(Year == 2020) %>%
  group_by(Month) %>%
  summarise(Time = sum(Listen) + sum(Read)) %>%
  pivot_longer(c(Time), names_to = "type", values_to = "val") %>%
  ggplot(aes(Month, val, label = val)) +
  geom_col(fill = "darkorange") +
  geom_text(vjust = 1.2,
            hjust = 'center',
            size = 3,
            color = 'white') +
  ylim(0, 3000) +
  scale_x_discrete(drop = F) +
  theme_hc() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "")
          
bar_meses_2021 <- data %>%
  filter(Year == 2021) %>%
  group_by(Month) %>%
  summarise(Time = sum(Listen) + sum(Read)) %>%
  pivot_longer(c(Time), names_to = "type", values_to = "val") %>%
  ggplot(aes(Month, val, label = val)) +
  geom_col(fill = "darkorange") +
  geom_text(vjust = 1.2,
            hjust = 'center',
            size = 3,
            color = 'white') +
  ylim(0, 3000) +
  scale_x_discrete(drop = F) +
  theme_hc() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "")

bar_meses_2022 <- data %>%
  filter(Year == 2022) %>%
  group_by(Month) %>%
  summarise(Time = sum(Listen) + sum(Read)) %>%
  pivot_longer(c(Time), names_to = "type", values_to = "val") %>%
  ggplot(aes(Month, val, label = val)) +
  geom_col(fill = "darkorange") +
  geom_text(vjust = 1.2,
            hjust = 'center',
            size = 3,
            color = 'white') +
  ylim(0, 3000) +
  scale_x_discrete(drop = F) +
  theme_hc() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "")
          

# Barras de progreso (no se usan por gauges)
# oraciones_totales <- data %>%
#   summarise(Sentences = sum(Sentences)) %>% 
#   mutate(type = "Sentences") %>% 
#   ggplot(aes(type, Sentences)) +
#   geom_col(aes("Sentences", 3500), fill = "grey70") +
#   geom_col(fill = "darkorange") +
#   annotate("text", 
#            x = 1.5, 
#            y = current_Sentences ,
#            colour = 'indianred2',
#            hjust = -0.2,
#            label = paste0(as.character(round(100*current_Sentences/3500,digits = 2)), "%")) +
#   annotate("text", 
#            x = 0.5, 
#            y = current_Sentences,
#            hjust = -0.2,
#            colour = 'indianred2',
#            label = as.character(current_Sentences)) +
#   geom_hline(yintercept = current_Sentences, linetype = 2) +
#   geom_hline(yintercept = 3500, linetype = 2) +
#   coord_flip() +
#   theme_minimal() +
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
#   labs(x = "", y = "", title = "")
# 
# oraciones_2022 <- data %>% 
#   filter(Year == 2022) %>%
#   summarise(Sentences = sum(Sentences)) %>% 
#   mutate(type = "Sentences") %>% 
#   ggplot(aes(type, Sentences)) +
#   geom_col(aes("Sentences", 180), fill = "grey70") +
#   geom_col(fill = "darkorange") +
#   annotate("text", 
#            x = 1.5, 
#            y = current_Sentences_2022,
#            hjust = -0.2,
#            colour = 'indianred2',
#            label = paste0(as.character(round(100*current_Sentences_2022/180,digits = 2)), "%")) +
#   annotate("text", 
#            x = 0.5, 
#            y = current_Sentences_2022,
#            hjust = -0.2,
#            colour = 'indianred2',
#            label = as.character(current_Sentences_2022)) +
#   geom_hline(yintercept = current_Sentences_2022, linetype = 2) +
#   geom_hline(yintercept = 180, linetype = 2) +
#   coord_flip() +
#   theme_minimal() +
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
#   labs(x = "", y = "", title = "")
#   

# Solo deja los gráficos
rm(current_Sentences)
rm(current_Sentences_2022)
rm(data)

rmarkdown::render("Dashboard.Rmd")
