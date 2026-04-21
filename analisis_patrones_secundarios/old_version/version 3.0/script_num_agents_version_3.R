#### Lectura de los datos y limpieza ####

# Se establece el espacio de trabajo y las librerías a utilizar
setwd("C:/Users/ulise/Documents/GitHub/Modelo-base-dinamica-de-opinion/experimentos/patrones secundarios/version 3.0")
library(tidyverse)

experimento_num_agents <- read.csv('experimento-agents-V3.csv', skip = 6)
experimento_num_agents <- experimento_num_agents %>%
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    pref_A = pref.A,
    pref_B = pref.B,
    agents_per_tick = agents.updated.per.tick,
    learning_rate = learning.rate,
    spatial_interactions = spatial.interactions.,
    confidence_threshold = confidence.threshold
  )

# Habiendose encontrado el nuevo valor optimo para learning rate con 5 agentes en
# el experimento anterior, ahora se evalua si hay un nuevo valor para el 
# numero de agentes que disminuya el error
experimento_num_agents <- experimento_num_agents[c('run_number', 'tick', 'agents_per_tick' ,'pref_A', 'pref_B')]

# Se especifica el nuevo numero de agentes totales, despues de las modificaciones
total_agents = 858

# Se obtiene el promedio de todas las repeticiones realizadas por cada combinacion de valores de los parametros
experimento_num_agents_avg <- experimento_num_agents %>%
  group_by(agents_per_tick, tick) %>%
  summarise(
    mean_pref_A = mean(pref_A),
    mean_pref_B = mean(pref_B),
    .groups = 'drop'
  ) %>%
  
  # Se agregan dos columnas par indicar el porcentaje de preferencia
  mutate(
    pct_A = (mean_pref_A / total_agents) * 100,
    pct_B = (mean_pref_B / total_agents) * 100
  )

#### Cálculo del error del modelo ####

# Se crea un dataframe con base en los datos de las encuestas reales para calcular el error
encuestas_reales <- data.frame(
  tick = c(69, 166, 259),
  pct_A_real = c(64, 61, 68),
  pct_B_real = c(36, 39, 32)
)

# Se calcula el error en base a las encuestas y el resultado final de la eleccion
experimento_num_agents_error <- experimento_num_agents_avg %>%
  # Se unen los valores de las encuestas reales a los experimentales
  inner_join(encuestas_reales, by = 'tick') %>%
  # Se calcula el error en cada una de las filas
  mutate(
    err_sq_A = (pct_A - pct_A_real)^2,
    err_sq_B = (pct_B - pct_B_real)^2,
    err_total_punto = (err_sq_A + err_sq_B) / 2
  ) %>%
  # Se agrupa el error en base al parámetro learning?rate
  group_by(agents_per_tick) %>%
  summarise(
    MSE = mean(err_sq_A),
    RMSE = sqrt(MSE),
    .groups = "drop"
  ) %>%
  # Se ordena de menor a mayor error
  arrange(MSE)

# Se muestran los mejores resultados
head(experimento_num_agents_error)

