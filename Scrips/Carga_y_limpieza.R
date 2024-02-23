# Limpieza y normalizacion
# Fecha: 25-01-2024
# Jorge A. Villegas Cruz

# Carga de librerias.
install.packages("tidyverse")
install.packages("skimr")

library(tidyverse)
library(data.table)
library(skimr)
library(dplyr)
library(readr)

# 1. Removemos notación científica.
options(scipen = 999)
# 2. Caracteres usados en el idioma español.
Sys.setlocale("LC_ALL", 'es_ES.UTF-8')

# Carga de datos.
trips_2023_01 <- read.csv("Datos/2023_01_tripdata.csv")
trips_2023_02 <- read.csv("Datos/2023_02_tripdata.csv")
trips_2023_03 <- read.csv("Datos/2023_03_tripdata.csv")
trips_2023_04 <- read.csv("Datos/2023_04_tripdata.csv")
trips_2023_05 <- read.csv("Datos/2023_05_tripdata.csv")
trips_2023_06 <- read.csv("Datos/2023_06_tripdata.csv")
trips_2023_07 <- read.csv("Datos/2023_07_tripdata.csv")
trips_2023_08 <- read.csv("Datos/2023_08_tripdata.csv")
trips_2023_09 <- read.csv("Datos/2023_09_tripdata.csv")
trips_2023_10 <- read.csv("Datos/2023_10_tripdata.csv")
trips_2023_11 <- read.csv("Datos/2023_11_tripdata.csv")
trips_2023_12 <- read.csv("Datos/2023_12_tripdata.csv")

#Revision de datos.
glimpse(trips_2023_01)
glimpse(trips_2023_02)
glimpse(trips_2023_03)
glimpse(trips_2023_04)
glimpse(trips_2023_05)
glimpse(trips_2023_06)
glimpse(trips_2023_07)
glimpse(trips_2023_08)
glimpse(trips_2023_09)
glimpse(trips_2023_10)
glimpse(trips_2023_11)
glimpse(trips_2023_12)


#/////////////  LIMPIEZA /////////////#

# Eliminacion de valores NA, valores vacios y duplicados.
trips_2023_01 <- trips_2023_01 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_02 <- trips_2023_02 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_03 <- trips_2023_03 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_04 <- trips_2023_04 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_05 <- trips_2023_05 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_06 <- trips_2023_06 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_07 <- trips_2023_07 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_08 <- trips_2023_08 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_09 <- trips_2023_09 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_10 <- trips_2023_10 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_11 <- trips_2023_11 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]
trips_2023_12 <- trips_2023_12 %>% drop_na() %>% unique() %>% .[rowSums(. == "") == 0, ]

# Revisar que las columnas sean iguales
compare_df_cols(trips_2023_01,
                trips_2023_02,
                trips_2023_03,
                trips_2023_04,
                trips_2023_05,
                trips_2023_06,
                trips_2023_07,
                trips_2023_08,
                trips_2023_09,
                trips_2023_10,
                trips_2023_11,
                trips_2023_12)

# Unir todos los data.frame.
trips_2023_year <- bind_rows(trips_2023_01,
                             trips_2023_02,
                             trips_2023_03,
                             trips_2023_04,
                             trips_2023_05,
                             trips_2023_06,
                             trips_2023_07,
                             trips_2023_08,
                             trips_2023_09,
                             trips_2023_10,
                             trips_2023_11,
                             trips_2023_12)
glimpse(trips_2023_year)

# Numero de filas
nrow(trips_2023_year)

# Se obtendra una muestra de la población.
# Nivel de confianza  : 95%
# Margen de error     : 5%
# Población           : 4331707
# Muestra             : 385

# Creación de data.frame con la muestra
trips_2023_muestra <- trips_2023_year[sample(nrow(trips_2023_year), 385), ] %>% 
  select(-ride_id,-started_at,-ended_at,)
skim_without_charts(trips_2023_muestra)
view(trips_2023_muestra)

# Verificar existencia de NA y duplicados.
any(is.na(trips_2023_muestra))
any(duplicated(trips_2023_muestra))
any(trips_2023_muestra == "")

# Cambio de formatos a formato Fecha hora de columnas started_at y ended_at
# Convertir a formato Date
trips_2023_muestra$started_at_date <- as.Date(dmy(trips_2023_muestra$started_at_date))
trips_2023_muestra$ended_at_date <- as.Date(dmy(trips_2023_muestra$ended_at_date))

#Convertir a formato Hora ITime
trips_2023_muestra$started_at_time <- as.ITime(trips_2023_muestra$started_at_time)
trips_2023_muestra$ended_at_time <- as.ITime(trips_2023_muestra$ended_at_time)
trips_2023_muestra$ride_length <- as.ITime(trips_2023_muestra$ride_length)

# Reemplazar los valores de 1 a 7 en la columna day_of_week por los nombres de los días de la semana
dias_semana <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
trips_2023_muestra <- trips_2023_muestra %>%
  mutate(day_of_week = factor(day_of_week, levels = 1:7, labels = dias_semana))

glimpse(trips_2023_muestra)
view(trips_2023_muestra)
