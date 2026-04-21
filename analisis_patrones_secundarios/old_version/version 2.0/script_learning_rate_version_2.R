#### Lectura de los datos y limpieza ####

# Se establece el espacio de trabajo y las librerías a utilizar
setwd("C:/Users/ulise/Documents/GitHub/Modelo-base-dinamica-de-opinion/experimentos/patrones secundarios/version 2.0")
library(tidyverse)

experimento_learning_rate <- read.csv('experimento-learning-rate.csv', skip = 6)
experimento_learning_rate <- experimento_learning_rate %>%
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

# Se quiere evaluar si existe un nuevo valor optimo para el parametro learning_rate
# despues de las modificaciones realizadas al modelos. Se seleccionan las variables
# de interes
experimento_learning_rate <- experimento_learning_rate[c('run_number', 'tick', 'learning_rate' ,'pref_A', 'pref_B')]

# Se especifica el nuevo numero de agentes totales, despues de las modificaciones
total_agents = 858

# Se obtiene el promedio de todas las repeticiones realizadas por cada combinacion de valores de los parametros
experimento_learning_rate_avg <- experimento_learning_rate %>%
  group_by(learning_rate, tick) %>%
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
experimento_learning_rate_error <- experimento_learning_rate_avg %>%
  # Se unen los valores de las encuestas reales a los experimentales
  inner_join(encuestas_reales, by = 'tick') %>%
  # Se calcula el error en cada una de las filas
  mutate(
    err_sq_A = (pct_A - pct_A_real)^2,
    err_sq_B = (pct_B - pct_B_real)^2,
    err_total_punto = (err_sq_A + err_sq_B) / 2
  ) %>%
  # Se agrupa el error en base al parámetro learning?rate
  group_by(learning_rate) %>%
  summarise(
    MSE = mean(err_sq_A),
    RMSE = sqrt(MSE),
    .groups = "drop"
  ) %>%
  # Se ordena de menor a mayor error
  arrange(MSE)

# Se muestran los mejores resultados
head(experimento_learning_rate_error)

