## INLFUENCIA POSITIVA ##

# Se evalua la capacided del modelo para reproducir el patron de voto general al activar
# la opcion de interaccion entre vecinos. Para ello, se realiza una exploracion de parametros
# similar a aquella realizada en el primer experimento

#### Lectura y limpieza de datos ####

# Se lee la base de datos, y se establecen los datos contra los que se va a comparar
# en base a las encuestas del grupo GEA-ISA ajustadas a solamente dos opciones
setwd("C:/Users/ulise/Documents/GitHub/Patrones secundarios para modelo basico de dinamica de opiniones/experimentos/modelo_vecinos_sexo/exploracion de parametros")
library(tidyverse)
encuestas_reales <- read.csv('datos_csv/patron_voto_general.csv')

vecinos_positiva_exploracion <- read.csv("datos_csv/exploracion-parametros-vecinos-positiva-table.csv", skip = 6)

# Se le cambian los nombres a los datos para manejarlos de forma mas sencilla
vecinos_positiva_exploracion <- vecinos_positiva_exploracion %>%
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    pref_A = pref.A,
    percentage_A = percentage.A,
    pairs_per_tick = pairs.per.tick,
    learning_rate = learning.rate,
    # confidence_threshold = confidence.threshold Este parametro no se utiliza en influencia positiva
  ) %>%  # Se seleccionan unicamente las variables de interes: los parametros, las preferencias
         # en el modelo, el tick o dia de la simulacion y el numero de ejecucion
    select(tick, run_number, pairs_per_tick, learning_rate, pref_A, percentage_A)

# Se filtran los datos correspondientes a la mejor configuracion, de forma que no se necesite volver a leerlos
best_pairs = 25
best_lr = 0.1

vecinos_positiva_best_configuration <- vecinos_positiva_exploracion %>% 
  filter(learning_rate == 0.1 & pairs_per_tick == 25)

# Se seleccionan unicamente los dias considerados en las encuestas
vecinos_positiva_exploracion <- vecinos_positiva_exploracion %>% 
  filter(tick == 69 | tick == 166 | tick == 259)


#### Calculo de RMSE para patron de preferencia general ####

# se calcula el RMSE, con base a los tres puntos de referencia. 
vecinos_positiva_error_by_run <- vecinos_positiva_exploracion %>% 
  inner_join(encuestas_reales, by = "tick") %>% # Se agregan las encuestas reales por tick
  mutate(            
    err_sq_A = (percentage_A - pct_A_real)^2 # Se calcula el MSE
  ) %>% 
  # Se agrupa en base a todos los parametros a considerar, y el numero de run para 
  # tener en cuenta las repeticiones
  group_by(run_number, pairs_per_tick, learning_rate) %>%  
  # Se calcula la raiz del error para cada repeticion de las combinacion de parametros
  summarise(
    RMSE_run = sqrt(mean(err_sq_A)),
    .groups = "drop"
  )

# Con base a las 30 repeticiones se sacan estadisticas
vecinos_positiva_error_mean <- vecinos_positiva_error_by_run%>% 
  # Se agrupan en base a los parametros
  group_by(pairs_per_tick, learning_rate) %>% 
  # Estadisticas de interes para cada combinacion, en base a 30 repeticiones
  summarise(
    mean_RMSE = mean(RMSE_run),
    sd_RMSE = sd(RMSE_run),
    min_RMSE = min(RMSE_run),
    max_RMSE = max(RMSE_run),
    .groups = "drop"
  ) 

vecinos_positiva_top_best <- vecinos_positiva_error_mean %>% 
  slice_min(mean_RMSE, n=5) %>% 
  mutate(etiqueta = paste0("Pairs:", pairs_per_tick, 
                           " | LR:", learning_rate)) %>% 
  mutate(etiqueta = reorder(etiqueta, -mean_RMSE))

#### Graficacion del error para cada combinacion de parametros ####

# Primera grafica. Error para todas las combinaciones de parametros
vecinos_positiva_error_plot <- ggplot(vecinos_positiva_error_mean, aes(x = pairs_per_tick, y = learning_rate, 
                                                             fill = mean_RMSE)) + 
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(
    title = "RMSE across 2 parameters for positive influence under neighbor interactions",
    x = "Learning rate",
    y = "Agents per tick",
    fill = "RMSE (%)"
  ) +
  theme_minimal() +
  theme(panel.spacing = unit(1, "lines")) 
print(vecinos_positiva_error_plot)

ggsave("figures/vecinos_positiva_error.png", plot = vecinos_positiva_error_plot,
       width = 8, height = 5, dpi = 300)



##### Graficacion de mejores configuraciones ####

# Grafica de paleta para las 5 configuraciones con el menor RMSE
vecinos_positiva_lollipop_plot <- ggplot(vecinos_positiva_top_best, aes(x = etiqueta, y = mean_RMSE)) +
  geom_errorbar(aes(ymin = mean_RMSE - sd_RMSE, ymax = mean_RMSE + sd_RMSE), 
                width = 0.3, color = "#2c3e50", linewidth = 0.8) +
  geom_point(size = 4, color = "#e67e22") + 
  coord_flip() +
  labs(
    title = "Top 5 configurations for positive influence under neighbor interaction",
    subtitle = "Ranked by lowest mean RMSE | Error bars show Standard Deviation (30 runs)",
    x = "Parameters (Pairs per tick | Learning Rate)",
    y = "Mean RMSE (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(family = "mono", size = 10), 
    plot.title = element_text(face = "bold")
  )

print(vecinos_positiva_lollipop_plot)
ggsave("figures/vecinos_positiva_lollipot.png", plot = vecinos_positiva_lollipop_plot,
       width = 8, height = 5, dpi = 300)


#### Analisis de mejor configuracion ####


# Se sacan las estadisticas en base a las 30 repeticiones para la mejor configuracion
best_RMSE = vecinos_positiva_top_best[1, ]$mean_RMSE

vecinos_positiva_mejor_config_avg <-  vecinos_positiva_best_configuration %>%  
  group_by(tick) %>% 
  summarise(mean_pct_A = mean(percentage_A),
            sd_pct_A = sd(percentage_A),
            max_pct_A = max(percentage_A),
            min_pct_A = min(percentage_A))

vecinos_positiva_best_distance <- encuestas_reales %>%
  inner_join(vecinos_positiva_mejor_config_avg, by = "tick") %>%
  mutate(
    diferencia = pct_A_real - mean_pct_A,
    y_centro = (pct_A_real + mean_pct_A) / 2 # Para centrar el texto en la línea
  ) %>% 
  select(tick, pct_A_real, diferencia, mean_pct_A, y_centro)

# Grafica para la evolucion de las preferencias en el tiempo =
vecinos_positiva_best_plot <- ggplot(vecinos_positiva_mejor_config_avg, aes(x = tick)) +
  geom_ribbon(aes(ymin = mean_pct_A - sd_pct_A, ymax = mean_pct_A + sd_pct_A), 
              fill = "brown", alpha = 0.2) +
  
  geom_line(aes(y = mean_pct_A), color = "brown4", size = 1) +
  
  geom_point(data = encuestas_reales, 
             aes(x = tick, y = pct_A_real, shape = "Datos reales"), 
             color = "red", size = 3, shape = 18) +
  
  geom_segment(data = vecinos_positiva_best_distance,
               aes(x = tick, xend = tick, y = pct_A_real, yend = mean_pct_A),
               linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(data = vecinos_positiva_best_distance,
            aes(x = tick, y = y_centro, 
                label = paste0(ifelse(diferencia > 0, "+", ""), round(diferencia, 1), " (%)")),
            hjust = +1.2, size = 3, fontface = "italic", color = "grey20") +
  
  theme_minimal() +
  labs(
    title = "Preference evolution for best configuration in positive influence,
    neighbor interactions",
    subtitle = paste0(c("Pairs per tick = ", "learning rate =  ", "Mean RMSE = "), 
                      c(best_pairs, best_lr, round(best_RMSE, 2)), collapse = ", "),
    x = "Ticks (days)",
    y = "Preference share for A (%)"
  ) +
  scale_y_continuous(limits = c(60,70), breaks = c(60:70))

print(vecinos_positiva_best_plot)
ggsave("figures/vecinos_positiva_best.png", plot = vecinos_positiva_best_plot,
       width = 8, height = 5, dpi = 300)


#### Histograma de opiniones para mejor configuracion ####

# Para una sola ejecucion, se lee el registro del histograma de opiniones, y se 
# le da el nombre adecuado a las variables

positiva_best_config_opinions <- read.csv(file = "positiva-best-config-opinions.csv")

initial_opinions <- positiva_best_config_opinions %>% 
  filter(tick == 0) %>% 
  mutate(initial_pref = ifelse(opinion >= 0, "Initial pref A", "Initial pref B")) %>% 
  select(agent_id, initial_pref)

positiva_best_config_opinions <- positiva_best_config_opinions %>% 
  left_join(initial_opinions, by = 'agent_id')

positiva_best_config_opinions <- positiva_best_config_opinions %>%
  mutate(
    opinion_change = (initial_pref == "Initial pref A" & opinion < 0) | 
      (initial_pref == "Initial pref B" & opinion > 0)
  ) %>%
  arrange(opinion_change)

final_tick <- max(positiva_best_config_opinions$tick)
n_total <- length(unique(positiva_best_config_opinions$agent_id))

opinion_change <- positiva_best_config_opinions %>%
  filter(tick == final_tick) %>%
  summarise(
    A_to_B = sum(initial_pref == "Initial pref A" & opinion < 0),
    B_to_A = sum(initial_pref == "Initial pref B" & opinion > 0)
  ) %>%
  mutate(
    pct_A_to_B = (A_to_B / n_total) * 100,
    pct_B_to_A = (B_to_A / n_total) * 100
  )

label_stats <- paste0(
  "Preference change A to B: ", round(opinion_change$pct_A_to_B, 1), "%\n",
  "Preference change B to A: ", round(opinion_change$pct_B_to_A, 1), "%"
)

# Se grafica la evolucion de las OPINIONES en el sistema.
positiva_best_config_opinions_plot <- ggplot(positiva_best_config_opinions,
                                             aes(x = tick, y = opinion, color = initial_pref)) +
  geom_point(aes(alpha = opinion_change, size = opinion_change)) + 
  scale_color_manual(values = c("Initial pref A" = "red", 
                                "Initial pref B" = "blue")) +
  scale_alpha_manual(values = c("FALSE" = 0.2, "TRUE" = 0.8), guide = "none") +
  scale_size_manual(values = c("FALSE" = 0.5, "TRUE" = 1.0), guide = "none") +
  annotate("label", 
           x = max(positiva_best_config_opinions$tick) * 0.95, y = -0.75, 
           label = label_stats, 
           fill = "white", alpha = 0.8, size = 3.5, fontface = "bold",
           hjust = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20", linewidth = 0.5) +
  labs(
    title = "Opinion evolution for best configuration in positive influence model",
    subtitle = paste0(c("Agents per tick = ", "learning rate =  "), positiva_best_parameters, collapse = ", "),
    x = "Ticks (days)",
    y = "Opinion Value [-1, 1]",
    color = "Initial preference"
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_minimal()

print(positiva_best_config_opinions_plot)
ggsave("positiva_best_config_opinions_plot.png", plot = positiva_best_config_opinions_plot,
       width = 8, height = 5, dpi = 300)
