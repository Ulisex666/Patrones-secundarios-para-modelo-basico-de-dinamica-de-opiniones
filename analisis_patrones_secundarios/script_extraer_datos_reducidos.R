# Este es el script utilizado para extraer la edad y sexo de aquellos considerados
# en las encuestas realizadas, para poder evaluar la capacidad del modelo de
# replicar patrones secundarios.

#### Lectura de los datos crudos de las encuestas ####
setwd("C:/Users/ulise/Documents/GitHub/Patrones secundarios para modelo basico de dinamica de opiniones/analisis_patrones_secundarios")
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
write_sav(primera_encuesta_reducida, "datos_sav/primera_encuesta_reducida.sav")
write_sav(segunda_encuesta_reducida, "datos_sav/segunda_encuesta_reducida.sav")
write_sav(tercera_encuesta_reducida, "datos_sav/tercera_encuesta_reducida.sav")


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
primera_encuesta_reducida <- read_sav(file = 'datos_sav/primera_encuesta_reducida.sav') 
segunda_encuesta_reducida <- read_sav(file = 'datos_sav/segunda_encuesta_reducida.sav') 
tercera_encuesta_reducida <- read_sav(file = 'datos_sav/tercera_encuesta_reducida.sav') 

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
  filter(preferencia == 1 | preferencia == 2) %>% 
  mutate(
    sexo = factor(sexo,
                  levels = c(1,2),
                  labels = c('Hombre', 'Mujer')),
    preferencia = factor(preferencia,
                         levels = c(1,2,3,4,7),
                         labels = c("Xochitl", "Sheinbaum", "Samuel", "Independiente", "Indefinido")
    ),
    edad = factor(edad,
                  levels = c(2,3,4,5),
                  labels = c("17-24", "25-39", "40-54", "55+"),
    )
  )

segunda_encuesta_dos_opciones <- segunda_encuesta_reducida %>% 
  filter(preferencia == 1 | preferencia == 2) %>% 
  mutate(
    sexo = factor(sexo,
                  levels = c(1,2),
                  labels = c('Hombre', 'Mujer')),
    preferencia = factor(preferencia,
                         levels = c(1,2,3,4,7),
                         labels = c("Xochitl", "Sheinbaum", "Samuel", "Independiente", "Indefinido")),
    edad = factor(edad,
                  levels = c(2,3,4,5),
                  labels = c("17-24", "25-39", "40-54", "55+"),
    )
  )

tercera_encuesta_dos_opciones <- tercera_encuesta_reducida %>% 
  filter(preferencia == 1 | preferencia == 2) %>% 
  mutate(
    sexo = factor(sexo,
                  levels = c(1,2),
                  labels = c('Hombre', 'Mujer')),
    preferencia = factor(preferencia,
                         levels = c(1,2,3,9),
                         labels = c("Xochitl", "Sheinbaum", "Samuel", "Indefinido")),
    edad = factor(edad,
                  levels = c(2,3,4,5),
                  labels = c("17-24", "25-39", "40-54", "55+"),
    )
  )

# Guardo los datos procesados para un manejo mas facil, asi no debo de andar
# repitiendo el proceso cada vez

write_sav(primera_encuesta_dos_opciones, "datos_sav/primera_dos_opciones.sav")
write_sav(segunda_encuesta_dos_opciones, "datos_sav/segunda_dos_opciones.sav")
write_sav(tercera_encuesta_dos_opciones, "datos_sav/tercera_dos_opciones.sav")

# Aqui verifico que si se correspondan mis porcentajes ajustados con aquellos
# obtenidos de la encuesta reducida

porcentajes_ajustados_primera <- primera_encuesta_dos_opciones %>% 
  mutate(peso = pond) %>% 
  group_by(preferencia) %>% 
  summarise(votos_ponderados = sum(peso)) %>% 
  mutate(porcentaje_oficial = (votos_ponderados/sum(votos_ponderados)) * 100)
print(porcentajes_ajustados_primera)

porcentajes_ajustados_segunda <- segunda_encuesta_dos_opciones %>% 
  mutate(peso = pond) %>% 
  group_by(preferencia) %>% 
  summarise(votos_ponderados = sum(peso)) %>% 
  mutate(porcentaje_oficial = (votos_ponderados/sum(votos_ponderados)) * 100)
print(porcentajes_ajustados_segunda)

porcentajes_ajustados_tercera <- tercera_encuesta_dos_opciones %>% 
  mutate(peso = pond) %>% 
  group_by(preferencia) %>% 
  summarise(votos_ponderados = sum(peso)) %>% 
  mutate(porcentaje_oficial = (votos_ponderados/sum(votos_ponderados)) * 100)
print(porcentajes_ajustados_tercera)
