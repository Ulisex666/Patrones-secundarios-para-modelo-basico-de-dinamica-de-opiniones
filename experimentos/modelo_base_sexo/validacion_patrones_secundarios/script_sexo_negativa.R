### INFLUENCIA NEGATIVA
### Este script se utiliza para validar el comportamiento del patron de voto
### por sexo obtenido al intentar replicar el cambio de opinion general observado
### en las encuestas. Para ello, se realizan 30 repeticiones tomando la mejor 
### combinacion de parametros obtenida en los primeros experimentos. 

setwd("C:/Users/ulise/Documents/GitHub/Patrones secundarios para modelo basico de dinamica de opiniones/validacion_patrones_secundarios")
library(tidyverse)
library(haven)
library(labelled)

#### Lectura y procesamiento de datos de la simulacion ####

# Leo la tabla de datos
patron_sexo_negativa <- read.csv("data_csv/patron-edad-negativa-table.csv", skip = 6)

# Se leen los datos empiricos de preferencia en el tiempo, para validar.
# Unicamente se considera la opcion A
encuestas_preferencias_general <- read.csv("data_csv/patron_voto_general.csv")
encuestas_preferencias_hombres <- read.csv("data_csv/encuestas_preferencias_A_hombres.csv") 
encuestas_preferencias_mujeres <- read.csv("data_csv/encuestas_preferencias_A_mujeres.csv")

# Cambio nombre de variables y selecciono unicamente las necesarias
patron_sexo_negativa <- patron_sexo_negativa %>% 
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    pref_A = pref.A,
    pct_A = percentage.A,
    pairs_per_tick = agents.updated.per.tick,
    learning_rate = learning.rate,
    male_pref_A = male.pref.A, 
    male_pct_A = male.percentage.A,
    female_pref_A = female.pref.A,
    female_pct_A = female.percentage.A,
  )

# Estos valores son fijos, por lo que no se necesitan leer de los datos
learning_rate = 0.1
pairs_per_tick = 20
confidence_threshold = 1.0
# Selecciono unicamente las variables de interes
patron_sexo_negativa <- patron_sexo_negativa %>% 
  select(tick, run_number, pct_A, male_pct_A, female_pct_A)


#### Extraccion de patrones ####

# Veo el patron general, el voto en toda la poblacion de la simulacion
patron_voto_general <- patron_sexo_negativa %>% 
  group_by(tick) %>% 
  summarise(mean_pct_A = mean(pct_A),
            sd_pct_A = sd(pct_A),
            max_pct_A = max(pct_A),
            min_pct_A = min(pct_A))

# Veo el patron secundario de voto en hombres en la simulacion
patron_voto_hombres <- patron_sexo_negativa %>% 
  group_by(tick) %>% 
  summarise(mean_hombre_A = mean(male_pct_A),
            sd_hombre_A = sd(male_pct_A),
            max_hombre_A = max(male_pct_A),
            min_hombre_A = min(male_pct_A))

# Veo el patron secundario de voto en mujeres en la simulacion
patron_voto_mujeres <- patron_sexo_negativa %>% 
  group_by(tick) %>% 
  summarise(mean_mujer_A = mean(female_pct_A),
            sd_mujer_A = sd(female_pct_A),
            max_mujer_A = max(female_pct_A),
            min_mujer_A = min(female_pct_A))

patron_general_distancia <- encuestas_preferencias_general %>%
  inner_join(patron_voto_general, by = "tick") %>%
  mutate(
    diferencia = pct_A_real - mean_pct_A,
    y_centro = (pct_A_real + mean_pct_A) / 2 
  ) %>% 
  select(tick, pct_A_real, diferencia, mean_pct_A, y_centro)

patron_hombres_distancia <- encuestas_preferencias_hombres %>%
  inner_join(patron_voto_hombres, by = "tick") %>%
  mutate(
    diferencia = porcentaje_votos - mean_hombre_A,
    y_centro = (porcentaje_votos + mean_hombre_A) / 2 
  ) %>% 
  select(tick, porcentaje_votos, diferencia, mean_hombre_A, y_centro) %>% 
  filter(tick != 0)


patron_mujeres_distancia <- encuestas_preferencias_mujeres %>%
  inner_join(patron_voto_mujeres, by = "tick") %>%
  mutate(
    diferencia = porcentaje_votos - mean_mujer_A,
    y_centro = (porcentaje_votos + mean_mujer_A) / 2 
  ) %>% 
  select(tick, porcentaje_votos, diferencia, mean_mujer_A, y_centro) %>% 
  filter(tick != 0)

##### Calculo del RMSE por sexo #####
# De la misma forma que se hizo para el patron principal, se calcula el RMSE
# para el patron secundario de preferencia entre hombres y entre mujeres

general_RMSE <- patron_sexo_negativa %>% 
  inner_join(encuestas_preferencias_general, by = "tick") %>% # Se agregan las encuestas reales por tick
  mutate( err_sq_A = (pct_A - pct_A_real)^2 # Se calcula el MSE
  ) %>% 
  group_by(run_number) %>% 
  summarise(rmse_run = sqrt(mean(err_sq_A)),
            .groups="drop")
mean_RMSE_general = mean(general_RMSE$rmse_run) 
sd_RMSE_general = sd(general_RMSE$rmse_run)


hombre_RMSE <- patron_sexo_negativa %>% 
  inner_join(encuestas_preferencias_hombres, by = "tick") %>% # Se agregan las encuestas reales por tick
  mutate( err_sq_hombre_A = (male_pct_A - porcentaje_votos)^2 # Se calcula el MSE
  ) %>% 
  group_by(run_number) %>% 
  summarise(rmse_run = sqrt(mean(err_sq_hombre_A)),
            .groups="drop")
mean_RMSE_hombres = mean(hombre_RMSE$rmse_run)
sd_RMSE_hombres = sd(hombre_RMSE$rmse_run)

mujer_RMSE <- patron_sexo_negativa %>% 
  inner_join(encuestas_preferencias_mujeres, by = "tick") %>% # Se agregan las encuestas reales por tick
  mutate( err_sq_hombre_A = (female_pct_A - porcentaje_votos)^2 # Se calcula el MSE
  ) %>% 
  group_by(run_number) %>% 
  summarise(rmse_run = sqrt(mean(err_sq_hombre_A)),
            .groups="drop")
mean_RMSE_mujeres = mean(mujer_RMSE$rmse_run)
sd_RMSE_mujeres = sd(mujer_RMSE$rmse_run)
#### Visualizacion de los resultados ####

# Grafica del cambio de voto general
patron_general_plot <- ggplot(patron_voto_general, aes(x = tick)) +
  geom_ribbon(aes(ymin = mean_pct_A - sd_pct_A, ymax = mean_pct_A + sd_pct_A), 
              fill = "brown", alpha = 0.2) +
  geom_line(aes(y = mean_pct_A), color = "brown4", size = 1) +
  geom_point(data = encuestas_preferencias_general, 
             aes(x = tick, y = pct_A_real, shape = "Datos reales"), 
             color = "red", size = 3, shape = 18) +
  geom_segment(data = patron_general_distancia,
               aes(x = tick, xend = tick, y = pct_A_real, yend = mean_pct_A),
               linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(data = patron_general_distancia,
            aes(x = tick, y = y_centro, 
                label = paste0(ifelse(diferencia > 0, "+", ""), round(diferencia, 1), " (%)")),
            hjust = +1.2, size = 3, fontface = "italic", color = "grey20") +
  theme_minimal() +
  labs(
    title = "Preference evolution and standard deviation in negative influence model",
    subtitle = paste0(c("Pairs per tick = ", "learning rate =  ", "confidence threshold = "), 
                      c(pairs_per_tick, learning_rate, confidence_threshold), collapse = ", "),
    x = "Ticks (days)",
    y = "Preference share for A (%)"
  ) +
  scale_y_continuous(limits = c(60,70), breaks = c(60:70))
print(patron_general_plot)

# Visualizacion del cambio de voto para hombres
patron_voto_hombres_plot <- ggplot(patron_voto_hombres, aes(x = tick)) +
  geom_ribbon(aes(ymin = mean_hombre_A - sd_hombre_A, ymax = mean_hombre_A + sd_hombre_A), 
              fill = "orange2", alpha = 0.2) +
  geom_line(aes(y = mean_hombre_A), color = "orange4", size = 1) +
  geom_point(data = encuestas_preferencias_hombres[2:3, ], 
             aes(x = tick, y = porcentaje_votos), 
             color = "orange", size = 3, shape = 18) +
  geom_segment(data = patron_hombres_distancia,
               aes(x = tick, xend = tick, y = porcentaje_votos, yend = mean_hombre_A),
               linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(data = patron_hombres_distancia,
            aes(x = tick, y = y_centro,
                label = paste0(ifelse(diferencia > 0, "+", ""), round(diferencia, 1), " (%)")),
            hjust = +1.2, size = 3, fontface = "italic", color = "grey20") +
  theme_minimal() +
  labs(
    title = "Preference share by men in negative influence",
    subtitle = paste0(c("Pairs per tick = ", "learning rate =  ", "confidence threshold = "),
                      c(pairs_per_tick, learning_rate, confidence_threshold), collapse = ", "),
    x = "Ticks (days)",
    y = "Male preference share for A (%)"
  ) +
  scale_y_continuous(limits = c(58,70), breaks = c(58:70))
print(patron_voto_hombres_plot)


# Visualizacion del cambio de voto para mujeres
patron_voto_mujeres_plot <- ggplot(patron_voto_mujeres, aes(x = tick)) +
  geom_ribbon(aes(ymin = mean_mujer_A - sd_mujer_A, ymax = mean_mujer_A + sd_mujer_A), 
              fill = "purple", alpha = 0.2) +
  geom_line(aes(y = mean_mujer_A), color = "purple4", size = 1) +
  geom_point(data = encuestas_preferencias_mujeres, 
             aes(x = tick, y = porcentaje_votos), 
             color = "purple", size = 3, shape = 18) +
  geom_segment(data = patron_mujeres_distancia,
               aes(x = tick, xend = tick, y = porcentaje_votos, yend = mean_mujer_A),
               linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(data = patron_mujeres_distancia,
            aes(x = tick, y = y_centro,
                label = paste0(ifelse(diferencia > 0, "+", ""), round(diferencia, 1), " (%)")),
            hjust = +1.2, size = 3, fontface = "italic", color = "grey20") +
  theme_minimal() +
  labs(
    title = "Preference share by women in negative influence",
    subtitle = paste0(c("Pairs per tick = ", "learning rate =  ", "confidence threshold = "), 
                      c(pairs_per_tick, learning_rate, confidence_threshold), collapse = ", "),
    x = "Ticks (days)",
    y = "Male preference share for A (%)"
  ) +
  scale_y_continuous(limits = c(60,75), breaks = c(60:75))
print(patron_voto_mujeres_plot)


# Comparacion del cambio de voto de los tres grupos a la vez
grafica_convergencia_negativa <- ggplot() +
  geom_ribbon(data = patron_voto_general, 
              aes(x = tick, ymin = mean_pct_A - sd_pct_A, ymax = mean_pct_A + sd_pct_A, fill = "General"), 
              alpha = 0.1) +
  geom_line(data = patron_voto_general, 
            aes(x = tick, y = mean_pct_A, color = "General"), linewidth = 1) +
  geom_ribbon(data = patron_voto_hombres, 
              aes(x = tick, ymin = mean_hombre_A - sd_hombre_A, ymax = mean_hombre_A + sd_hombre_A, fill = "Men"), 
              alpha = 0.1) +
  geom_line(data = patron_voto_hombres, 
            aes(x = tick, y = mean_hombre_A, color = "Men"), linewidth = 1) +
  geom_ribbon(data = patron_voto_mujeres, 
              aes(x = tick, ymin = mean_mujer_A - sd_mujer_A, ymax = mean_mujer_A + sd_mujer_A, fill = "Women"), 
              alpha = 0.1) +
  geom_line(data = patron_voto_mujeres, 
            aes(x = tick, y = mean_mujer_A, color = "Women"), linewidth = 1) +
  scale_color_manual(values = c("General" = "brown4", "Men" = "orange3", "Women" = "purple3")) +
  scale_fill_manual(values = c("General" = "brown3", "Men" = "orange2", "Women" = "purple2")) +
  labs(
    title = "Preference evolution by gender under negative influence",
    subtitle = "Comparison of trajectories across 30 repetitions",
    x = "Ticks (Days)",
    y = "Preference Share for Option A (%)",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))
print(grafica_convergencia_negativa)
ggsave("figures/negative_influence_evolution_gender.png", plot = grafica_convergencia_negativa,
       width = 8, height = 5, dpi = 300)

patron_diferencia_genero <- data.frame(
  tick = patron_voto_hombres$tick,
  gap_mean = patron_voto_hombres$mean_hombre_A - patron_voto_mujeres$mean_mujer_A,
  gap_sd = sqrt(patron_voto_hombres$sd_hombre_A^2 + patron_voto_mujeres$sd_mujer_A^2)
)

encuestas_diferencia_genero <- data.frame(
  tick = encuestas_preferencias_hombres$tick,
  diferencia_genero = encuestas_preferencias_hombres$porcentaje_votos 
  - encuestas_preferencias_mujeres$porcentaje_votos
)

diferencia_voto_genero_plot <- ggplot(patron_diferencia_genero, aes(x = tick, y = gap_mean)) +
  geom_ribbon(aes(ymin = gap_mean - gap_sd, ymax = gap_mean + gap_sd), 
              fill = "purple", alpha = 0.2) +
  geom_line(color = "purple4", linewidth = 1) +
  geom_point(data = encuestas_diferencia_genero, aes(x = tick, y = diferencia_genero)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(
    title = "Difference in preference for Option A (Men - Women)",
    x = "Ticks (Days)",
    y = "Percentage Point Difference",
  ) +
  coord_cartesian(ylim = c(-5, 5)) # Ajusta según lo que observes en tus datos
print(diferencia_voto_genero_plot)

