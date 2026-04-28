# En este script lo que hago es extraer el otro patron secundario a evaluar:
# preferencias por grupo de edad. Para ello, utilizo los mismos datos que tengo
# para el patron secundario de edad, filtrados tomando unicamente los dos candidatos
# principales


#### Lectura de los datos y limpieza ####
setwd("C:/Users/ulise/Documents/GitHub/Patrones secundarios para modelo basico de dinamica de opiniones/analisis_patrones_secundarios")
library(tidyverse)
library(haven)
library(labelled)

primera_dos_opciones <- read_sav("datos_sav/primera_dos_opciones.sav") %>% 
  as_factor() %>% 
  mutate(preferencia = fct_relevel(preferencia, "Sheinbaum", "Xochitl"))

segunda_dos_opciones <- read_sav("datos_sav/segunda_dos_opciones.sav") %>% 
  as_factor() %>% 
  mutate(preferencia = fct_relevel(preferencia, "Sheinbaum", "Xochitl"))

tercera_dos_opciones <- read_sav("datos_sav/tercera_dos_opciones.sav") %>% 
  as_factor() %>% 
  mutate(preferencia = fct_relevel(preferencia, "Sheinbaum", "Xochitl"))

# Reviso la proporcion de entrevistados por rango de edad en cada encuesta, tomando
# en cuenta el ponderador. En teoria, deberian de mantenerse igual o casi igual en 
# las tres encuestas

# Primera encuesta
primera_pct_edad <- primera_dos_opciones %>% 
  group_by(edad) %>% 
  summarise(total_individuos = sum(pond)) %>% 
  mutate(pct_por_edad_ponderado = (total_individuos / sum(total_individuos)) * 100)

# Segunda encuesta
segunda_pct_edad <- segunda_dos_opciones %>% 
  group_by(edad) %>% 
  summarise(total_individuos = sum(pond)) %>% 
  mutate(pct_por_edad_ponderado = (total_individuos / sum(total_individuos)) * 100)

# Tercera encuesta 
tercera_pct_edad <- tercera_dos_opciones %>% 
  group_by(edad) %>% 
  summarise(total_individuos = sum(pond)) %>% 
  mutate(pct_por_edad_ponderado = (total_individuos / sum(total_individuos)) * 100)

#### Patron secundario: preferencias por edades ####
# Ahora reviso la preferencia por rango de edad en cada encuesta. Divido por
# grupo de edad, y en cada grupo veo cuantos preferian a Sheinbaum o a Xochitl

# Primera encuesta
primera_preferencia_por_edad <- primera_dos_opciones %>%
  group_by(edad, preferencia) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Segunda encuesta
segunda_preferencia_por_edad <- segunda_dos_opciones %>%
  group_by(edad, preferencia) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Tercera encuesta
tercera_preferencia_por_edad <- tercera_dos_opciones %>%
  group_by(edad, preferencia) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Ahora se revisa al reves, primero divido en base a partidos politicos y despues
# lo divido por edades

# Primera encuesta
primera_preferencia_por_partido <- primera_dos_opciones %>%
  group_by(preferencia, edad) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Segunda encuesta
segunda_preferencia_por_partido <- segunda_dos_opciones %>%
  group_by(preferencia, edad) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Tercera encuesta
tercera_preferencia_por_partido <- tercera_dos_opciones %>%
  group_by(preferencia, edad) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

#### Visualizacion de los resultados #### 
# Se hacen tres imagenes de grafica de pastel, una por cada encuesta. Se toman 
# los datos filtrando primero por partido y despues por edad, dado que eso da
# una mejor visualizacion al reducir el numero de opciones

# Grafica de primera encuesta
primera_preferencia_por_edad_plot <- ggplot(primera_preferencia_por_edad, 
                                            aes(x = "", y = porcentaje_votos, fill = preferencia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ edad) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "B", "Sheinbaum" = "A")) +
  theme_void() + 
  labs(
    title = "Preference share by age",
    subtitle = "First survey",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(size = 12, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14)
  )
print(primera_preferencia_por_edad_plot)

primera_preferencia_por_partido_plot <- ggplot(primera_preferencia_por_partido, 
                                            aes(x = "", y = porcentaje_votos, fill = edad)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ preferencia, 
             labeller = as_labeller(c("Xochitl" = "B", "Sheinbaum" = "A"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  theme_void() + 
  labs(
    title = "Preference share by age",
    subtitle = "First survey",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(size = 12, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14),
  )
print(primera_preferencia_por_partido_plot)

# Grafica de segunda encuesta
segunda_preferencia_por_edad_plot <- ggplot(segunda_preferencia_por_edad, 
                                            aes(x = "", y = porcentaje_votos, fill = preferencia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ edad) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "B", "Sheinbaum" = "A")) +
  theme_void() + 
  labs(
    title = "Preference share by age",
    subtitle = "Second survey",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(size = 12, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14)
  )
print(segunda_preferencia_por_edad_plot)

segunda_preferencia_por_partido_plot <- ggplot(segunda_preferencia_por_partido, 
                                               aes(x = "", y = porcentaje_votos, fill = edad)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ preferencia, 
             labeller = as_labeller(c("Xochitl" = "B", "Sheinbaum" = "A"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  theme_void() + 
  labs(
    title = "Preference share by age",
    subtitle = "Second survey",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(size = 12, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14),
  )
print(segunda_preferencia_por_partido_plot)


# Grafica de tercera encuesta
tercera_preferencia_por_edad_plot <- ggplot(tercera_preferencia_por_edad, 
                                            aes(x = "", y = porcentaje_votos, fill = preferencia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ edad) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "B", "Sheinbaum" = "A")) +
  theme_void() + 
  labs(
    title = "Preference share by age",
    subtitle = "Third survey",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(size = 12, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14)
  )
print(tercera_preferencia_por_edad_plot)

tercera_preferencia_por_partido_plot <- ggplot(tercera_preferencia_por_partido, 
                                               aes(x = "", y = porcentaje_votos, fill = edad)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ preferencia, 
             labeller = as_labeller(c("Xochitl" = "B", "Sheinbaum" = "A"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  theme_void() + 
  labs(
    title = "Preference share by age",
    subtitle = "Third survey",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(size = 12, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14),
  )
print(tercera_preferencia_por_partido_plot)


#### Evolucion de preferencias por edad en el tiempo ####
# Ahora evaluo el cambio de preferencia por edad en el tiempo, de una forma
# mas entendible
library(ggrepel)
evolucion_preferencias_edad <- bind_rows(
  primera_preferencia_por_edad %>% mutate(encuesta = "Survey 1"),
  segunda_preferencia_por_edad %>% mutate(encuesta = "Survey 2"),
  tercera_preferencia_por_edad %>% mutate(encuesta = "Survey 3")
) %>% 
  mutate(encuesta = factor(encuesta, levels = c("Survey 1", "Survey 2", "Survey 3")))

evolucion_opcion_a <- evolucion_preferencias_edad %>% filter(preferencia == "Sheinbaum")
evolucion_opcion_a_plot <- ggplot(evolucion_opcion_a, aes(x = encuesta,
                              y = porcentaje_votos, color = edad, group = edad)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = paste0(round(porcentaje_votos, 1), "%")),
                  color = "brown4",
                  size = 4, 
                  fontface = "bold",
                  box.padding = 0.5, 
                  point.padding = 0.5,
                  show.legend = FALSE) +
  scale_y_continuous(limits = c(min(evolucion_opcion_a$porcentaje_votos) - 5,
                                max(evolucion_opcion_a$porcentaje_votos) + 10)) +
  labs(
    title = "Preference Evolution: Option A",
    subtitle = "Comparison between age groups",
    x = "Survey Period",
    y = "Preference Share (%)",
    color = "Age group"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        legend.position = "bottom")
print(evolucion_opcion_a_plot)
ggsave("figures/evolution_preference_share_A_age.png", plot = evolucion_opcion_a_plot,
       width = 8, height = 5, dpi = 300)


evolucion_opcion_b <- evolucion_preferencias_edad %>% filter(preferencia == "Xochitl")
evolucion_opcion_b_plot <- ggplot(evolucion_opcion_b, aes(x = encuesta,
                                                          y = porcentaje_votos, color = edad, group = edad)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = paste0(round(porcentaje_votos, 1), "%")),
                  color = "steelblue2",
                  size = 4, 
                  fontface = "bold",
                  box.padding = 0.5, 
                  point.padding = 0.5,
                  show.legend = FALSE) +
  scale_y_continuous(limits = c(min(evolucion_opcion_b$porcentaje_votos) - 5,
                                max(evolucion_opcion_b$porcentaje_votos) + 10)) +
  labs(
    title = "Preference Evolution: Option B",
    subtitle = "Comparison between age groups",
    x = "Survey Period",
    y = "Preference Share (%)",
    color = "Age group"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        legend.position = "bottom")
print(evolucion_opcion_b_plot)
ggsave("figures/evolution_preference_share_B_age.png", plot = evolucion_opcion_b_plot,
       width = 8, height = 5, dpi = 300)
