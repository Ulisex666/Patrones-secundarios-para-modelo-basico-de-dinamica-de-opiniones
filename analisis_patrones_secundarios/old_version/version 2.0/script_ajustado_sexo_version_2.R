#### Lectura de los datos y limpieza ####

# Se establece el espacio de trabajo y las librerías a utilizar
setwd("C:/Users/ulise/Documents/GitHub/Modelo-base-dinamica-de-opinion/experimentos/patrones secundarios/version 2.0")
library(tidyverse)

experimento_secundario_sexo <- read.csv('datos_experimentos/experimento-ajustado-sexo.csv', skip = 6)
experimento_secundario_sexo <- experimento_secundario_sexo %>%
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    agents_per_tick = agents.updated.per.tick,
    learning_rate = learning.rate,
    spatial_interactions = spatial.interactions.,
    confidence_threshold = confidence.threshold,
    hombres_opcion_A = hombres.opcion.A,
    mujeres_opcion_A = mujeres.opcion.A
  )

# Se quiere evaluar el patron secundario de voto por sexo. Para ello, se evalua
# el total de hombres que votan por la opcion A y tambien de mujeres que votan
# por la opcion A
experimento_secundario_sexo <- experimento_secundario_sexo[c('run_number', 'hombres_opcion_A', 'mujeres_opcion_A')]

# Se especifica el nuevo numero de agentes totales, despues de las modificaciones
total_agents = 858
total_hombres = 438
total_mujeres = 420

#### Patron secundario: sexo ####

# Se agrupa la distribucion de la preferencia de voto de acuerdo al sexo
# Se toma solo la opcion A. Se calcula el promedio, el maximo y el minimo en 
# las 30 repeticiones
preferencias_A_hombres<- experimento_secundario_sexo %>%
  summarise(
    promedio_hombres_A = mean(hombres_opcion_A),
    max_hombres_A = max(hombres_opcion_A),
    min_hombres_A = min(hombres_opcion_A)
  ) 

preferencias_A_mujeres<- experimento_secundario_sexo %>%
  summarise(
    promedio_mujeres_A = mean(mujeres_opcion_A),
    max_mujeres_A = max(mujeres_opcion_A),
    min_mujeres_A = min(mujeres_opcion_A)
  ) 

preferencias_A_mujeres_pct <- preferencias_A_mujeres %>%
  mutate(
    category = "Women (option A)",
    promedio_pct = promedio_mujeres_A / total_mujeres * 100,
    max_pct = max_mujeres_A / total_mujeres * 100,
    min_pct = min_mujeres_A / total_mujeres * 100
  ) %>%
  select(-promedio_mujeres_A, -min_mujeres_A, -max_mujeres_A)

preferencias_A_hombres_pct <- preferencias_A_hombres %>%
  mutate(
    category = "Women (option A)",
    promedio_pct = promedio_hombres_A / total_hombres * 100,
    max_pct = max_hombres_A / total_hombres * 100,
    min_pct = min_hombres_A / total_hombres * 100
  ) %>%
  select(-promedio_hombres_A, -min_hombres_A, -max_hombres_A)

ggplot(preferencias_A_mujeres_pct, aes(x = category, y = promedio_pct)) +
  
  geom_linerange(aes(ymin = min_pct, ymax = max_pct), 
                 linewidth = 1.5, color = "#2c3e50") +
  
  geom_point(size = 5, color = "#2c3e50") +
  
  geom_hline(yintercept = 60, linetype = "dashed", color = "#e74c3c", linewidth = 1.2) +
  
  annotate("text", x = 1, y = 58, label = "Observed Real Value (60%)", 
           color = "#e74c3c", fontface = "bold") +
  
  annotate("text", x = 1, y = 66, label = "Simulated value (mean = 65 %)", 
           color = "grey", fontface = "bold") +
  
  labs(
    title = "Secondary pattern: Women preference share for option A at third poll
    with adjusted parameters",
    y = "Preference Percentage (%)",
    x = ""
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 80, 10)) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 12, face = "bold")
  )


ggplot(preferencias_A_hombres_pct, aes(x = category, y = promedio_pct)) +
  
  geom_linerange(aes(ymin = min_pct, ymax = max_pct), 
                 linewidth = 1.5, color = "#2c3e50") +
  
  geom_point(size = 5, color = "#2c3e50") +
  
  geom_hline(yintercept = 60, linetype = "dashed", color = "#e74c3c", linewidth = 1.2) +
  
  annotate("text", x = 1, y = 58, label = "Observed Real Value (60%)", 
           color = "#e74c3c", fontface = "bold") +
  
  annotate("text", x = 1, y = 66, label = "Simulated value (mean = 65 %)", 
           color = "grey", fontface = "bold") +
  
  labs(
    title = "Secondary pattern: Men preference share for option A at third poll
    with adjusted parameters",
    y = "Preference Percentage (%)",
    x = ""
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 76), breaks = seq(0, 80, 10)) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 12, face = "bold")
  )

