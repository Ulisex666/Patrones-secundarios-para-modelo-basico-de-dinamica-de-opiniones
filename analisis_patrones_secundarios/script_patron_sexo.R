
#### Patron secundario: preferencia por sexo ####
## Ahora aqui si ya checo el patron secundario en base al sexo. Primero, divido
# en base al sexo y despues en base a las preferencias electorales. Asi, para cada
# encuesta puedo ver cuantos hombres querian votar por Morena y cuantos por el PAN,
# lo mismo para mujeres
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

## Primero reviso la proporcion por sexo en cada encuesta. En teoria deberia de
## matenerse al considerar el ponderador

# Primera encuesta
primera_pct_sexos <- primera_dos_opciones %>% 
  group_by(sexo) %>% 
  summarise(total_individuos = sum(pond)) %>% 
  mutate(pct_por_sexo_ponderado = (total_individuos / sum(total_individuos)) * 100)

# Segunda encuesta
segunda_pct_sexos <- segunda_dos_opciones %>% 
  group_by(sexo) %>% 
  summarise(total_individuos = sum(pond)) %>% 
  mutate(pct_por_sexo_ponderado = (total_individuos / sum(total_individuos)) * 100)

# Tercera encuesta 
tercera_pct_sexos <- tercera_dos_opciones %>% 
  group_by(sexo) %>% 
  summarise(total_individuos = sum(pond)) %>% 
  mutate(pct_por_sexo_ponderado = (total_individuos / sum(total_individuos)) * 100)

# Ahora si, reviso la preferencia de cada sexo por encuesta. Me da tres muestras,
# en la primera, la segunda y la tercera

# Primera encuesta
primera_preferencia_por_sexo <- primera_dos_opciones %>%
  group_by(sexo, preferencia) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Segunda encuesta
segunda_preferencia_por_sexo <- segunda_dos_opciones %>%
  group_by(sexo, preferencia) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Tercera encuesta
tercera_preferencia_por_sexo <- tercera_dos_opciones %>%
  group_by(sexo, preferencia) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Ahora lo hago al reves. Primero divido por partido, despues por sexo. Esto me
# dice para cada encuesta, de los que quieren votar por Morena, cuantos son hombres 
# y cuantos mujeres. Lo mismo para PAN.

# Primera encuesta
primera_preferencia_por_partido <- primera_dos_opciones %>%
  group_by(preferencia, sexo) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Segunda encuesta
segunda_preferencia_por_partido <- segunda_dos_opciones %>%
  group_by(preferencia, sexo) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Tercera encuesta
tercera_preferencia_por_partido <- tercera_dos_opciones %>%
  group_by(preferencia, sexo) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

#### Graficas de los resultados####
# Ahora hago las graficas de pastel de los resultados. Son 3 graficas, dividiendo
# por sexo y despues por preferencia

# Grafica para la primera encuesta
primera_preferencia_por_sexo_plot <- ggplot(primera_preferencia_por_sexo, aes(x = "", y = porcentaje_votos, fill = preferencia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ sexo, 
             labeller = as_labeller(c("Hombre" = "Men", "Mujer" = "Women"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "B", "Sheinbaum" = "A")) +
  theme_void() + 
  labs(
    title = "Preference share by gender",
    subtitle = "First poll",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(size = 12, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14)
  )
print(primera_preferencia_por_sexo_plot)
ggsave("figures/first_poll_preference_share_gender.png", plot = primera_preferencia_por_sexo_plot,
       width = 8, height = 5, dpi = 300)

# Graficas para la segunda encuesta
segunda_preferencia_por_sexo_plot <- ggplot(segunda_preferencia_por_sexo, aes(x = "", y = porcentaje_votos, fill = preferencia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ sexo, 
             labeller = as_labeller(c("Hombre" = "Men", "Mujer" = "Women"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "B", "Sheinbaum" = "A")) +
  theme_void() + 
  labs(
    title = "Preference share by gender",
    subtitle = "Second poll",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14),
    strip.text = element_text(size = 12, face = "bold"), 
    #legend.position = "bottom"
  )
print(segunda_preferencia_por_sexo_plot)
ggsave("figures/second_poll_preference_share_gender.png", plot = segunda_preferencia_por_sexo_plot,
       width = 8, height = 5, dpi = 300)

# Graficas para la tercera encuesta
tercera_preferencia_por_sexo_plot <- ggplot(tercera_preferencia_por_sexo, aes(x = "", y = porcentaje_votos, fill = preferencia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ sexo, 
             labeller = as_labeller(c("Hombre" = "Men", "Mujer" = "Women"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "B", "Sheinbaum" = "A")) +
  theme_void() + 
  labs(
    title = "Preference share by gender",
    subtitle = "Third poll",
    fill = "Options"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = 14),
    strip.text = element_text(size = 12, face = "bold"), 
    #legend.position = "bottom"
  )
print(tercera_preferencia_por_sexo_plot)
ggsave("figures/third_poll_preference_share_gender.png", plot = tercera_preferencia_por_sexo_plot,
       width = 8, height = 5, dpi = 300)

#### Evolucion de las preferencias en el tiempo ####
# De forma similar a los primeros experimentos, quiero ver ahora el cambio de
# preferencias electorales en el tiempo, con una grafica para los hombres y 
# otra para las mujeres. Debo de tener en cuenta que NO tengo los resultados de
# la eleccion por genero de momento.

# Se combinan todas las tablas en una sola para su visualizacion
evolucion_preferencias_sexo <- bind_rows(
  primera_preferencia_por_sexo %>% mutate(encuesta = "Survey 1"),
  segunda_preferencia_por_sexo %>% mutate(encuesta = "Survey 2"),
  tercera_preferencia_por_sexo %>% mutate(encuesta = "Survey 3")
  ) %>% 
  mutate(encuesta = factor(encuesta, levels = c("Survey 1", "Survey 2", "Survey 3"))) 


# Aqui filtro para ver el cambio unicamente en hombres y en mujeres
evolucion_preferencias_hombre <- evolucion_preferencias_sexo %>% 
  filter(sexo == "Hombre")
evolucion_preferencias_mujer <- evolucion_preferencias_sexo %>% 
  filter(sexo == "Mujer")

# Grafico el cambio para hombres
evolucion_preferencias_hombre_plot <- ggplot(evolucion_preferencias_hombre, 
                              aes(x = encuesta, y = porcentaje_votos, 
                                  color = preferencia, group = preferencia)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            vjust = -1.8, size = 4, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(
    values = c("Sheinbaum" = "brown4", "Xochitl" = "steelblue"),
    labels = c("Sheinbaum" = "Option A", "Xochitl" = "Option B")
  ) +
  scale_y_continuous(limits = c(min(evolucion_preferencias_hombre$porcentaje_votos) - 5, 
                                max(evolucion_preferencias_hombre$porcentaje_votos) + 10)) +
  labs(
    title = "Preference Evolution: Men",
    subtitle = "Comparison of Option A (red) and Option B (blue) across surveys",
    x = "Survey Period",
    y = "Preference Share (%)",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )
print(evolucion_preferencias_hombre_plot)
ggsave("figures/evolution_preference_share_men.png", plot = evolucion_preferencias_hombre_plot,
       width = 8, height = 5, dpi = 300)

# Grafico el cambio para mujeres
evolucion_preferencias_mujer_plot <- ggplot(evolucion_preferencias_mujer, 
                                             aes(x = encuesta, y = porcentaje_votos, 
                                                 color = preferencia, group = preferencia)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            vjust = -1.8, size = 4, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(
    values = c("Sheinbaum" = "brown4", "Xochitl" = "steelblue"),
    labels = c("Sheinbaum" = "Option A", "Xochitl" = "Option B")
  ) +
  scale_y_continuous(limits = c(min(evolucion_preferencias_hombre$porcentaje_votos) - 5, 
                                max(evolucion_preferencias_hombre$porcentaje_votos) + 10)) +
  labs(
    title = "Preference Evolution: Men",
    subtitle = "Comparison of Option A (red) and Option B (blue) across surveys",
    x = "Survey Period",
    y = "Preference Share (%)",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )
print(evolucion_preferencias_mujer_plot)
ggsave("figures/evolution_preference_share_women.png", plot = evolucion_preferencias_mujer_plot,
       width = 8, height = 5, dpi = 300)
