# Protocolo ODD para modelo base de dinámica de opiniones
A continuación se encuentra la descripción del modelo base de dinámica de opiniones, siguiendo el protocolo ODD.

# Propósito y patrones

El propósito del modelo es la simulación de la elección presidencial mexicana llevada a cabo en el 2024, mediante el uso de dinámica de opiniones. En este contexto, *una opinión es la posición que se tiene sobre un tema, y se formaliza como un número que cambia entre dos extremos*. Se espera que las personas modifiquen sus opiniones después de interactuar con sus pares a través del efecto de la influencia social. Se consideran tres tipos de influencia diferentes entre los agentes durante de una interacción.

- **Influencia positiva.** Después de una interacción, dos agentes siempre tendrán una opinión más cercana a la del otro. Esto toma como base teorías cognitivas que hacen hincapié en el papel del aprendizaje social y de la presión social para seguir las normas de un grupo.

- **Confianza acotada.** Un agente es influenciado a tomar una opinión más cercana a la del otro agente en la interacción solamente si sus opiniones son suficientemente similares. Qué tan parecida debe ser la opinión de otro agente está determinado por una cota o límite de confianza. La base teórica principal es el sesgo de confirmación, la tendencia de preferir información que este de acuerdo con lo que ya se opina y evadir aquella que contradiga nuestras creencias.

- **Influencia negativa.** Si dos agentes con opiniones muy disimilares interactúan entre ellos, se influencian en sentido opuesto, tomando opiniones todavía más dispares. Esto toma como base teórica efectos como la xenofobia o el rechazo a grupos percibidos como externos, aunque la evidencia empírica de este efecto es mixta.

La opinión es convertida a voto dependiendo de su valor, tomando como referencia los dos candidatos principales en la elección real. De esta forma, el principal patrón a evaluar es la evolución del voto durante todo el periodo de la simulación, con el objetivo de replicar la distribución de voto observada en la elección real. Se utiliza una serie de encuestas de opinión realizadas durante el periodo electoral como refencia.

# Entidades, variables de estado y escalas

Las únicas entidades del modelo son las parcelas, indicando un votante dentro de las encuestas de opinión tomadas como referencia. Cada una se encuentra definida por una variable de estado: su opinión en un momento dado. Esta es un valor real en el rango *[-1,1]*, donde 1 indica una opinión completamente a favor del candidato A y -1 una opinión completamente a favor del candidato B. Cuando se activa el parámetro *local-interactions?* cada agente puede comunicarse únicamente con sus cuatro vecinos más cercanos, por lo que esto se vuelve otra característica que define su comportamiento.

Se utiliza un mundo cerrado de tamaño *107x10*, con cada parcela representando un votante de la encuestas. De esta forma, se tiene 1070 agentes dentro del modelo. El alcance temporal del modelo se da en base al número de días desde la primera encuestas hasta el día de la elección, con un total de 260 días. Cada tick representa un día, con el modelo ejecutándose un total de 260 ticks.

# Descripción general y *scheduling*

El modelo realiza las siguientes acciones por cada paso de tiempo.

**Selección de agentes iniciales**. Se selecciona a una cantidad de parcelas definida por el parámetro *agents-updated-per-tick* de manera aleatoria de entre todas las presentes en el modelo. 

**Selección de agentes para interacción.** Cada uno de los *n* agentes iniciales selecciona a otro agente en el modelo para interactuar. Si se tiene activado el parámetro *spatial-interactions?*, cada agente inicial debe seleccionar a uno de sus vecinos. En caso contrario, selecciona un agente al azar de todos los presentes en el modelo, permitiendo repeticiones.

**Interacción entre agentes.** Los agentes cambian su opinión de acuerdo al tipo de inlfuencia seleccionado en la simulación, detallados en la sección de submodelos. Si se cumplen las condiciones adecuadas, ambos agentes presentes en la simulación modifican el valor de su opinión.

**Actualización de las preferencias**. Al terminar las interacciones entre los seleccionados, se actualizan las preferencias de voto del modelo de acuerdo al cambio de las opiniones.

**Visualización.** El color de los agentes es modificado de acuerdo al valor de su opinión, y se actualizan las gráficas y visualizaciones en el modelo.

**Terminación**. Al llegar al tick 260, el modelo actualiza las preferencias y las visualizaciones por una última vez, terminando su ejecución.


# Conceptos de diseño

- **Principios básicos.** La suposición básica en todo modelo de dinámica de opinión es que la influencia social tiene un papel fundamental a a la hora de formar y modificar opiniones. En este caso se esta consideran tres diferentes tipos de influencia al interactuar con otros agentes, cada una con diferentes justificaciones en ciencias sociales y psicología. Al activar las interacciones espaciales, se esta asumiendo que el efecto de la influencia social se da únicamente por aquellos considerados cercanos. 

- **Emergencia.** Los patrones principales a buscar son la distribución de opiniones a escala global en el sistema y la distribución de la preferencia de votos emergente de esta. Estos patrones emergen de la interacción entre pares de los agentes del sistema y del tipo de influencia seleccionado.

- **Adaptación.** Los agentes adaptan su opinión después de interactuar con otros agentes de acuerdo al tipo de influencia seleccionado en la simulación. Para una influencia positiva, el agente busca parecerse más a cualquier otra persona después de una interacción. En el caso de confianza acotada, busca solamente parecerse a aquellos similares a sí mismo. Para la influencia negativa, busca alejar su opinión de aquellas que sean muy disimilares.

- **Objetivos.** Los tres modelos de influencia asumen que un agente busca ajustar su opinión de acuerdo a las interacciones con otros. Se difiere en qué agentes son considerados suficientemente importantes como para afectar la opinión, y el sentido en que se afecta esta opinión.

- **Aprendizaje.** Este concepto no se utiliza en el modelo.

- **Predicción.** Este concepto no se utiliza en el modelo.

- **Percepciones.** En la interacción entre agentes, se asume que cada agente es completamente consciente de la opinión del otro.

- **Interacciones.** La interacción a modelar es el compartir opinión entre pares y cómo estas opiniones son modificadas de acuerdo al tipo de influencia considerado. Las interacciones se dan de forma directa, donde la opinión de un agente afecta la del otro de acuerdo al tipo de interacción.

- **Estocásticidad.** Las opiniones iniciales de cada agente se dan de forma aleatoria, siguiendo una distribución uniforme. Los agentes a interactuar en cada paso de tiempo se seleccionan de manera aleatoria.

- **Colectivos.** Este concepto no se utiliza en el modelo.

- **Observaciones.** Los agentes cambian su color de acuerdo a la opinión que tengan en ese momento, con valores entre -1 y 1. Aquellos con valores cercanos a -1 tomarán un color azul intenso, mientras que aquellos con valores cercanos a 1 tomarán colores rojos.También se tiene un histograma de las opiniones en todo el sistema, junto a una gráfica que muestra su evolución cada 10 pasos de tiempo. Para la visualización de las preferencias de voto se cuenta con dos gráficos, uno que indica el porcentaje de preferencia por A mientras que la segunda muestra las preferencias por B, siendo actualizadas cada paso de tiempo.

# Inicialización

El modelo se inicializa asignando las opiniones iniciales a los agentes de acuerdo al parámetro *percent-option-B*, indicando el porcentaje de agentes con preferencia por la opción B. Para la comparación con las encuestas de opinión utilizadas, este parámetro debe tomar el valor de 39. Las opiniones iniciales para los agentes con preferencia por A se generan con una distribución uniforme con valores entre *[0,1]*, mientras que las opiniones para agentes con preferencia por B toman valores de una distribución uniforme entre *[-1,0]*. 

Posteriormente, se actualiza el color de los agentes de acuerdo a la opinión asignada, y se actualizan las preferencias de voto por la opción A y por la opción B.

# Datos de entrada

El modelo no necesita del uso de datos de entrada.

# Submodelos

## Influencia positiva
 Para dos agentes i y j con opiniones x1, x2 respectivamente, su opinión se modifica después de una interacción mediante la siguiente fórmula 

```
let x1-new (x1 + learning-rate * (x2 - x1))
    let x2-new (x2 + learning-rate * (x1 - x2))
    set opinion x1-new
      ask other-patch [ set opinion x2-new] 
```

donde el parámetro *learning-rate* indica que tan rápido cambia la opinión de un     agente después de una interacción.

## Confianza acotada
Para dos agentes i y j con opiniones x1, x2 respectivamente, su opinión se modifica después de una interacción mediante la siguiente fórmula 

```
if (abs (x1 - x2) < confidence-threshold) [ 
      let x1-new (x1 + learning-rate * (x2 - x1))
      let x2-new (x2 + learning-rate * (x1 - x2))
      set opinion x1-new
      ask other-patch [ set opinion x2-new]
```
  El nuevo parámetro a considerar es *confidence-threshold*, indicando la tolerancia que tiene cada agente para opiniones disimilares. El parámetro *learning-rate* actúa de la misma forma que en el modelo de influencia positiva. De esta manera, un agente ajusta su opinión solamente si interactua con otro agente con una opinión lo suficientemente cercana a la que ya posee. En caso contrario, la opinión de ambos agentes en la interacción se mantiene igual.

## Influencia negativa
Para dos agentes i y j con opiniones x1, x2 respectivamente, su opinión se modifica después de una interacción mediante la siguiente fórmula

```
if (abs (x1 - x2) < confidence-threshold) [ 
      let x1-new (x1 + learning-rate * (x2 - x1))
      let x2-new (x2 + learning-rate * (x1 - x2))
      set opinion x1-new
      ask other-patch [ set opinion x2-new]
      ]
      if (abs (x1 - x2) > confidence-threshold)[ 
        ifelse (x1 > x2)
        [
          let x1-new (x1 + learning-rate * (x1 - x2) * (1 - x1) * .5)
          let x2-new (x2 + learning-rate * (x2 - x1) * (1 + x2) * .5)
          set opinion x1-new
          ask other-patch [ set opinion x2-new]
        ]
        [
          let x1-new (x1 + learning-rate * (x1 - x2) * (1 + x1) * .5)
          let x2-new (x2 + learning-rate * (x2 - x1) * (1 - x2) * .5)
          set opinion x1-new
          ask other-patch [ set opinion x2-new]
        ]
      ]
```

En este caso la interacción se ve dominada por el parámetro *confidence-threshold*. Si la diferencia de las opiniones es menor al valor de *confidence-threshold*, entonces ambos ajustan su opinión para ser más cercanas de acuerdo al valor del *learning-rate*. Sin embargo, si la diferencia entre opiniones es mayor a el valor de *confidence-threshold*, entonces ambos agentes alejan sus opiniones de acuerdo al valor del parámetro *learning-rate*.


## Conversión de opinión a preferencia de voto
Para convertir la opinión a preferencia de voto, se toma una opinión mayor a cero como la preferencia por el candidato A, y una opinión negativa como preferencia por el candidato B. De esta forma, para actualizar las preferencias globales de los agentes presentes en el modelo se tiene la siguiente fórmula.

```
  set pref-A (count patches with [opinion > 0] )
  set pref-B (count patches with [opinion < 0] )
``` 

El total de agentes con opinión mayor a cero se cuenta como el total de la población con la intención de votar por la opción A, mientra que el total de agentes con opinión menor a cero se cuenta como el número de agentes con la intención de votar por la opción B.


## Referencias

Trabajo basado en los modelos de influencia positivam, negativa y acotada encontrados en:

* Paul Smaldino. (2024). psmaldino/modsoc: Modeling Social Behavior (v1.0). Zenodo. https://doi.org/10.5281/zenodo.11245354