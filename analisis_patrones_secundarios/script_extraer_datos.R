# Este es el script utilizado para extraer la edad y sexo de aquellos considerados
# en las encuestas realizadas, para poder evaluar la capacidad del modelo de
# replicar patrones secundarios

#### Lectura de los datos crudos de las encuestas ####
setwd("C:/Users/ulise/Documents/GitHub/Modelo-base-dinamica-de-opinion/experimentos/patrones secundarios")
library(tidyverse)
library(haven)
library(labelled)

# Se leen en memoria las tres bases de datos, las encuestas realizadas por el grupo 
# GEA-ISA sobre la intencion de voto en las elecciones mexicanas a nivel presidencial
# en el 2024
primera_encuesta = read_sav("datos_sav/primera_encuesta.sav")
segunda_encuesta = read_sav("datos_sav/segunda_encuesta.sav")
tercera_encuesta = read_sav("datos_sav/tercera_encuesta.sav")

# Vemos los atributos en la pregunta relacionada a la intencion de voto, clasificada
# por alianza de los partidos
attr(primera_encuesta$R73b, "labels") # En la primera encuesta, se tiene en la pregunta R61
attr(segunda_encuesta$R85, "labels") # En la segunda encuesta, se tiene en la pregunta R74
attr(tercera_encuesta$R78, "labels") # En la tercera encuesta, se tiene en la pregunta R71

# Se seleccionan las variables relacionadas a la localidad, edad y sexo de cada entrevistado,
# ademas de la preferencia electoral registrada
# Tambien se va a extraer el ponderador para poder tener validez con respecto a la
# preferencia por genero en la encuesta

primera_encuesta_reducida <- primera_encuesta %>%
  select(folio, entidad, region, municip, seccion, edad, sexo, R73b, pond, fexpm)

segunda_encuesta_reducida <- segunda_encuesta %>%
  select(folio, entidad, region, edad, sexo, R85, pond, fexpm)

tercera_encuesta_reducida <- tercera_encuesta %>%
  select(folio, unidad, region, edad, sexo, R78, pond, fexpm)

# se guardan los datos filtrados en un archivo SAV
write_sav(primera_encuesta_reducida, "primera_encuesta_reducida.sav")
write_sav(segunda_encuesta_reducida, "segunda_encuesta_reducida.sav")
write_sav(tercera_encuesta_reducida, "tercera_encuesta_reducida.sav")


#### Evaluacion de los ponderadores ####

# A ver que suma cada uno de los ponderadores considerados en las encuestas
pesos_sum <- primera_encuesta %>% 
  summarise(
    sum_expansion = sum(fexpm, na.rm = T),
    sum_ponderador = sum(pond, na.rm = T),
    n_entrevistas = n()
  )


# Ahora intentare replicar los resultados obtenidos en base a el ponderador 
# demografico, en cuanto al porcentaje reportado en la encuesta para todas las
# opciones
porcentajes_preferencia <- primera_encuesta %>% 
  mutate(peso = pond) %>% 
  group_by(R73b) %>% 
  summarise(votos_ponderados = sum(peso)) %>% 
  mutate(porcentaje_oficial = (votos_ponderados/sum(votos_ponderados)) * 100)

#### Filtrado por opciones principales ####
# Empiezo leyendo las bases de datos reducidas, para evitar problemas
primera_encuesta_reducida <- read_sav(file = 'primera_encuesta_reducida.sav') 
segunda_encuesta_reducida <- read_sav(file = 'segunda_encuesta_reducida.sav') 
tercera_encuesta_reducida <- read_sav(file = 'tercera_encuesta_reducida.sav') 

# Cambio los nombres de las variables, de forma que pueda manejar de manera mas
# sencilla las bases de datos
primera_encuesta_reducida <- primera_encuesta_reducida %>% 
  rename(preferencia = R73b)
segunda_encuesta_reducida <- segunda_encuesta_reducida %>% 
  rename(preferencia = R85)
tercera_encuesta_reducida <- tercera_encuesta_reducida %>% 
  rename(preferencia = R78)

# Filtro para quedarme unicamente con los votantes que tienen intencion de voto
# por las opciones principales: Morena o PAN. Ademas, le cambio el nombre a los
# valores tomados por las variables para mayor legibilidad

primera_encuesta_dos_opciones <- primera_encuesta_reducida %>% 
  filter(preferencias == 1 | preferencias == 2) %>% 
  mutate(
    sexo = factor(sexo,
                  levels = c(1,2),
                  labels = c('Hombre', 'Mujer')),
    preferencia = factor(preferencias,
                          levels = c(1,2,3,4,7),
                          labels = c("Xochitl", "Sheinbaum", "Samuel", "Independiente", "Indefinido")
                          ),
    edad = factor(edad,
                  levels = c(2,3,4,5),
                  labels = c("17-24", "25-39", "40-54", "55+"),
    )
  )

segunda_encuesta_dos_opciones <- segunda_encuesta_reducida %>% 
  filter(preferencias == 1 | preferencias == 2) %>% 
  mutate(
    sexo = factor(sexo,
                  levels = c(1,2),
                  labels = c('Hombre', 'Mujer')),
    preferencia = factor(preferencias,
                          levels = c(1,2,3,4,7),
                          labels = c("Xochitl", "Sheinbaum", "Samuel", "Independiente", "Indefinido")),
    edad = factor(edad,
                  levels = c(2,3,4,5),
                  labels = c("17-24", "25-39", "40-54", "55+"),
    )
  )

tercera_encuesta_dos_opciones <- tercera_encuesta_reducida %>% 
  filter(preferencias == 1 | preferencias == 2) %>% 
  mutate(
    sexo = factor(sexo,
                  levels = c(1,2),
                  labels = c('Hombre', 'Mujer')),
    preferencia = factor(preferencias,
                          levels = c(1,2,3,9),
                          labels = c("Xochitl", "Sheinbaum", "Samuel", "Indefinido")),
    edad = factor(edad,
                  levels = c(2,3,4,5),
                  labels = c("17-24", "25-39", "40-54", "55+"),
    )
  )

# Guardo los datos procesados para un manejo mas facil, asi no debo de andar
# repitiendo el proceso cada vez

write_sav(primera_encuesta_dos_opciones, "primera_dos_opciones.sav")
write_sav(segunda_encuesta_dos_opciones, "segunda_dos_opciones.sav")
write_sav(tercera_encuesta_dos_opciones, "tercera_dos_opciones.sav")

# Aqui verifico que si se correspondan mis porcentajes ajustados con aquellos
# obtenidos de la encuesta reducida

porcentajes_ajustados_primera <- primera_encuesta_dos_opciones %>% 
  mutate(peso = pond) %>% 
  group_by(preferencias) %>% 
  summarise(votos_ponderados = sum(peso)) %>% 
  mutate(porcentaje_oficial = (votos_ponderados/sum(votos_ponderados)) * 100)
print(porcentajes_ajustados_primera)

porcentajes_ajustados_segunda <- segunda_encuesta_dos_opciones %>% 
  mutate(peso = pond) %>% 
  group_by(preferencias) %>% 
  summarise(votos_ponderados = sum(peso)) %>% 
  mutate(porcentaje_oficial = (votos_ponderados/sum(votos_ponderados)) * 100)
print(porcentajes_ajustados_segunda)

porcentajes_ajustados_tercera <- tercera_encuesta_dos_opciones %>% 
  mutate(peso = pond) %>% 
  group_by(preferencias) %>% 
  summarise(votos_ponderados = sum(peso)) %>% 
  mutate(porcentaje_oficial = (votos_ponderados/sum(votos_ponderados)) * 100)
print(porcentajes_ajustados_tercera)

#### Patron secundario: preferencia por sexo ####
## Ahora aqui si ya checo el patron secundario en base al sexo. Primero, divido
# en base al sexo y despues en base a las preferencias electorales. Asi, para cada
# encuesta puedo ver cuantos hombres querian votar por Morena y cuantos por el PAN,
# lo mismo para mujeres

primera_dos_opciones <- read_sav("primera_dos_opciones.sav") %>% 
  as_factor()
segunda_dos_opciones <- read_sav("segunda_dos_opciones.sav") %>% 
  as_factor()
tercera_dos_opciones <- read_sav("tercera_dos_opciones.sav") %>% 
  as_factor()

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
  group_by(sexo, preferencias) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Segunda encuesta
segunda_preferencia_por_sexo <- segunda_dos_opciones %>%
  group_by(sexo, preferencias) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Tercera encuesta
tercera_preferencia_por_sexo <- tercera_dos_opciones %>%
  group_by(sexo, preferencias) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Ahora lo hago al reves. Primero divido por partido, despues por sexo. Esto me
# dice para cada encuesta, de los que quieren votar por Morena, cuantos son hombres 
# y cuantos mujeres. Lo mismo para PAN.

# Primera encuesta
primera_preferencia_por_partido <- primera_dos_opciones %>%
  group_by(preferencias, sexo) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Segunda encuesta
segunda_preferencia_por_partido <- segunda_dos_opciones %>%
  group_by(preferencias, sexo) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

# Tercera encuesta
tercera_preferencia_por_partido <- tercera_dos_opciones %>%
  group_by(preferencias, sexo) %>%
  summarise(votos_ponderados = sum(pond, na.rm = TRUE), 
            .groups = "drop_last") %>% 
  mutate(porcentaje_votos = (votos_ponderados / sum(votos_ponderados)) * 100)

#### Graficas de los resultados####
# Ahora hago las graficas de pastel de los resultados. Seran 6 graficas, empezando
# con un grupo de tres indicando el cambio de preferencia dividiendo entre hombres
# y mujeres en las tres encuestas

# Grafica para la primera encuesta
primera_preferencia_por_sexo_plot <- ggplot(primera_preferencia_por_sexo, aes(x = "", y = porcentaje_votos, fill = preferencias)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ sexo, 
             labeller = as_labeller(c("Hombre" = "Men", "Mujer" = "Women"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "A", "Sheinbaum" = "B")) +
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

# Graficas para la segunda encuesta
segunda_preferencia_por_sexo_plot <- ggplot(segunda_preferencia_por_sexo, aes(x = "", y = porcentaje_votos, fill = preferencias)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ sexo, 
             labeller = as_labeller(c("Hombre" = "Men", "Mujer" = "Women"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "A", "Sheinbaum" = "B")) +
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

# Graficas para la tercera encuesta
tercera_preferencia_por_sexo_plot <- ggplot(tercera_preferencia_por_sexo, aes(x = "", y = porcentaje_votos, fill = preferencias)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ sexo, 
             labeller = as_labeller(c("Hombre" = "Men", "Mujer" = "Women"))) +
  geom_text(aes(label = paste0(round(porcentaje_votos, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Xochitl" = "steelblue", "Sheinbaum" = "brown4"),
                    labels = c("Xochitl" = "A", "Sheinbaum" = "B")) +
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

evolucion_preferencias_hombre <- evolucion_preferencias_sexo %>% 
  filter(sexo == "Hombre")

evolucion_preferencias_mujer <- evolucion_preferencias_sexo %>% 
  filter(sexo == "Mujer")

evolucion_preferencias_hombre_plot <- ggplot(evolucion_preferencias_hombre, 
                              aes(x = encuesta, y = porcentaje_votos, 
                                  color = preferencias, group = preferencias)) +
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

evolucion_preferencias_mujer_plot <- ggplot(evolucion_preferencias_mujer, 
                                             aes(x = encuesta, y = porcentaje_votos, 
                                                 color = preferencias, group = preferencias)) +
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
