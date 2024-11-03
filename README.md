# SFA RShiny WebApp
 Web application written on Shiny (R) to compute different stochastic frontier analysis (SFA) models 

# EJECUTAR LA APLICACIÓN WEB ONLINE
El código fuente de la aplicación web que acompaña a la memoria de este trabajo se encuentra en la carpeta stochastic-frontier-analysis.

Actualmente, la aplicación web está alojada online en

https://fidelsolis.shinyapps.io/stochastic-frontier-analysis/


# EJECUTAR LA APLICACIÓN DE MANERA LOCAL
Si por lo contrario desea ejecutar la aplicación web de manera local debe seguir los siguientes pasos:
- Instale R en su dispositivo.
- Instale los paquetes 'shiny', 'readODS', 'xlsx', 'plm' 'sfaR', 'frontier'. Quizá requiera que actualice paquetes que ya tiene 
  instalado.
- Ejecute el siguiente comando:
	 shiny::runApp('camino/hacia/stochastic-frontier-analysis')
	
  Alternativcamente, si utiliza RStudio aparecerá un botón "Run App" si tiene abierto el archivo "server.R" o "ui.R".


# DESCRIPCIÓN DE LOS ARCHIVOS DE EJEMPLO
Actualmente la aplicación web contiene cuatro archivos de ejemplo.


------------------------------------------------
    charnes1981.csv
------------------------------------------------
Datos obtenidos de un programa financiado por EEUU para proveer asistencia a alumnos de educación primaria desaventajados. Las 50 
primeras muestras pertenecen al programa PFT. Las 20 últimas observaciones no pertenecen al programa.

## Columnas:
### firm
  Número de escuela

### education_mother
  nivel de educación de la madre
### highest_occupation
  ocupación más alta de un miembro de la familia
### parental_visits
  visitas parentales a la escuela
### parent_counseling
  tiempo que pasan los padres con materias escolares
### n_teachers
  número de profesores en la escuela
### reading
  puntuación de compresión lectora
### maths 
  puntuación de matemáticas
### sesteem
  puntuación de autoestima
### participation
  =1 si pertence al programa PFT, =0 si no pertenece al programa PFT
### name
  localización de la escuela

Citación: Charnes, A., Cooper, W. W. y Rhodes, E. (1978). ‘Evaluating Program and Managerial Efficiency: An Application of Data 
Envelopment Analysis to Program Follow Through’. Management Science 27.6. Publisher: INFORMS, pags. 668-697.


------------------------------------------------
    pib.csv
------------------------------------------------
Datos de panel de un conjunto de países a lo largo de 40 años.

## Columnas:
### year
  Año
### cid
  Identificador del país (númerico)
### code
  Identificador del país (texto)
### country
  Nombre del país
### y
  PIB del país
### k
  Capital físico del país
### kbc30
  Proyección del capital físico del país 30 años antes
### lwdi
  Labor/trabajo
### secondary
  Años de educación secundaria
### egc
  Capacidad de generación eléctrica
### mlines
  Líneas de teléfono principales
### troads
  Número total de carreteras
### cells
  Número de teléfonos
### proads
  Número de carreteras pavimentadas
### rails
  Número de raíles

Citación: Calderón, C., Moral-Benito, E., Servén, L. (2015): Is infrastructure capital productive? A dynamic heterogeneous approach 
(replication data). Version: 1. Journal of Applied Econometrics.


------------------------------------------------
    elsalvador.csv
------------------------------------------------
Datos del Programa Ambiental de El Salvador (PAES), un programa que promueve diversificación de cultivmos y prácticas de 
conservación del suelo.

## Columnas:
### loutput
  logaritmo del valor de producción (en dólares)
### lseeds
  logaritmo del valor de las semillas usadas (en dólares)
### lland
  logaritmo de área cultivada (en manzanas, donde una manzana son 0.7 hectáreas) 
### llabour
  logaritmo del número total de trabajadores
### lfertilizer
  logaritmo del valor de fertilizantes usados (en dólares)
### lpesticide
  logaritmo del valor de pesticidas usados (en dólares)
### diste1
  logaritmo de la distancia del epicentro del terremoto de 2001 (en kilómetros)
### participation
  =1 si pertence al programa PAES, =0 si no pertenece al programa PAES

Citación: Centorrino, Samuele; Perez-Urdiales, Maria; Bravo-Ureta, Boris E.; Wall, Alan (2023): Binary endogenous treatment in 
stochastic frontier models with an application to soil conservation in El Salvador (replication data). 
Version: 1. Journal of Applied Econometrics.


------------------------------------------------
    rice_production_philippines.csv
------------------------------------------------
Datos obtenidos de 43 pequeños productores de arroz en la región Tarlac de las Filipinas entre 1990 y 1997.

## Columnas:
### YEARDUM
  Año (1=1990, ..., 8=1997)
### FMERCODE
  Identificador del granjero
### PROD
  Output (en toneladas de arroz)
### AREA
  Area cultivada en hectáreas
### LABOR
  Labor/trabajo usado (en días de trabajo de personas contratadas y familia)
### NPK
  Fertilizante usado (en kilogramos)
### OTHER
  Otros inputs usados
### PRICE
  Precio del output (pesos filipinos por kilogramo)
### AREAP
  Precio del área (pesos filipinos por hectárea)
### LABORP
  Precio del labor (pesos filipinos por día de trabajo)
### NPKP
  Precio del fertilizante (pesos filipinos por kilogramo)
### OTHERP
  Precio de otros inputs
### AGE
  Edad del líder de la granja (años)
### EDYRS
  Educación del líder de la granja (años)
### HHSIZE
  Tamaño de la granja (nº de personas)
### NADULT
  Número de adultos en la granja
### BANRAT
  Porcentaje de área clasificada como campos en meseta.


Citación: Coelli, T. J., Rao, D. S. P., O’Donnell, C. J., and Battese, G. E. (2005) An Introduction to Efficiency and 
Productivity Analysis, Springer, New York.
