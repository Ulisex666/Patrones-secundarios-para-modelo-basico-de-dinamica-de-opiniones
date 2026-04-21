# Este es el script utilizado para generar el archivo que puede leer NetLogo para
# generar a los agentes con sus caracteristicas demograficas basadas en 
# las encuestas

setwd("C:/Users/ulise/Documents/GitHub/Modelo-base-dinamica-de-opinion/experimentos/patrones secundarios")
library(tidyverse)
library(haven)
library(labelled)
library(dplyr)

# Se lee la base de datos ya filtrada por unicamente aquellos que quieren votar
# por los candidatos principales

pref_principales_primera <- read_sav("datos_sav/pref_principales_primera.sav")

# Se tiene un total de 858 observaciones, mientras que el modelo considera 1070

# Se generan las coordenadas para los 1070 patches
x_coords <- -53:53  # Eje X de -53 a 53
y_coords <- 4:-5    # eje y de -5 a 4

patches_coord <- expand.grid(x = x_coords, y = y_coords)

# Se generan columnas de 0 para los patches sin datos
n_faltante <- nrow(patches_coord) - nrow(pref_principales_primera)
sobrantes <- pref_principales_primera %>%
  slice(rep(1, n_faltante)) %>% 
  mutate(across(everything(), ~ 0))

datos_completos <- bind_rows(pref_principales_primera, sobrantes)

datos_finales <- bind_cols(patches_coord, datos_completos)

write.table(datos_finales,
            file = 'initial_agent_data.txt',
            sep = " ",
            row.names = F,
            col.names = F)
