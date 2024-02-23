# Carga de librerias
install.packages("tidyverse")
install.packages("skimr")

library(tidyverse)
library(skimr)
library(dplyr)
library(readr)

view(trips_2023_muestra)

# Descartados
ride_id
started_at
ended_at
start_station_id
end_station_id
start_lat
start_lng
end_lat
end_lng

# cuantitativos
started_at_date
ended_at_date
started_at_time
ended_at_time
day_of_week

# cualitativos
member_casual
rideable_type
start_station_name
end_station_name

skim(trips_2023_muestra)

head(trips_2023_muestra)
glimpse(trips_2023_muestra)
summary(trips_2023_muestra)

# 1. ¿En qué se diferencian los socios anuales y los ciclistas ocasionales con 
# respecto al uso de las bicicletas de Cyclistic?

# Numero de usuarios, el tiempo promedio de viaje
trips_2023_muestra %>%
  group_by(member_casual) %>%
  summarize(
    viajes = n(),
    media = mean(ride_length),
    mediana = median(ride_length),
    Des_estandar = sd(ride_length)
  )

table(trips_2023_muestra$member_casual) %>% 
pie()

# Dias mas transcurridos de los usuarios en la semana
ggplot(data = trips_2023_muestra, mapping = aes(x = as.factor(day_of_week), fill = as.factor(day_of_week))) +
  geom_bar() +
  facet_wrap(~member_casual) +
  labs(title = "Días de Mayor Movimiento Semanal",
       subtitle = "Dias de la semana con mas viajes realizados",
       x = "Dias de la semana",
       y = "Número de viajes",
       fill = "Dias de la semana"
       ) +
  theme(axis.text.x = element_blank())


#Preferecia del tipo de bicicletas de cada usuario.
trips_2023_muestra %>% 
  group_by(member_casual) %>% 
  summarize(
      suma = table(rideable_type),
      bicis = levels(rideable_type),
      tipo_bicicleta = unique(rideable_type)
  )

#Graficando los valores para una mejor comprensión.
ggplot(data = trips_2023_muestra, 
       mapping = aes(x = rideable_type, label = rideable_type, fill = rideable_type)) +
  geom_bar() + 
  facet_grid(~member_casual) +
  labs(title = "Preferencia de bicicletas",
       subtitle = "Preferencia de los usuarios con las bicicletas",
       x = "Tipos de bicicletas", y = "Conteo") +
  geom_text(aes(label=..count..), stat='count',  #Agrega los valores sobre las barras
            position=position_dodge(0.5),
            vjust=-0.2, 
            size=3.5
  )

# Buscar los lugares mas recurrentes para realizar las publicaciones.

# Estaciones mas usadas de los miembros casuales.
# Las 5 estaciones donde se ha comenzado los viajes
start_stations <- trips_2023_muestra %>%
  dplyr::filter(member_casual == "casual") %>%
  dplyr::select(start_station_name) %>%
  table() %>% 
  data.frame() %>% rename(stations = start_station_name, conteo = Freq) %>% 
  arrange(desc(.$conteo))

# Las 5 estaciones donde se han terminado los viajes
end_statios <- trips_2023_muestra %>%
  dplyr::filter(member_casual == "casual") %>%
  dplyr::select(end_station_name) %>%
  table() %>% 
  data.frame() %>% rename(stations = end_station_name, conteo = Freq) %>% 
  arrange(desc(.$conteo))

# Uniendo todas las estaciones.
top_stations <- bind_rows(start_stations, end_statios) %>%
  group_by(stations) %>%
  summarize(valor = sum(conteo)) %>%
  arrange(desc(valor)) %>% 
  data.frame() %>% slice(0:10)
  
# reordenar los valores de mayor a menor.
top_stations$stations <- reorder(top_stations$stations, top_stations$valor)

# o

# estaciones mas usadas por los miembros casuales.
top_stations <- 
  trips_2023_muestra %>%
  dplyr::filter(member_casual == "casual") %>%
  select(start_station_name, end_station_name) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "stations") %>%
  group_by(stations) %>%
  count() %>%
  top_n(5, wt = n) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  data.frame() %>% rename(valor = n) %>% 
  slice(0:10)

# reordenar los valores de mayor a menor.
top_stations$stations <- reorder(top_stations$stations, top_stations$valor)

# Graficar las estaciones mas usadas.
ggplot(top_stations, aes(x = stations, y = valor)) +
  geom_bar(stat = "identity", fill = "#27408B") +
  geom_text(aes(label = stations, hjust = ifelse(valor < mean(valor), -0.2, 1.2)), size = 4, color = ifelse(top_stations$valor > mean(top_stations$valor), "white", "black")) +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  labs(x = "Estaciones",
       y = "Frecuencia")

  
