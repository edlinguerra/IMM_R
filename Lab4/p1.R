# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # ACTIVIDAD 4: Análisis de Clasificación  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   

# Caso Ekofisk macrofauna y contaminantes en sedimento

rm(list=ls()) #limpiar el ambiente de trabajo

setwd("C:/Users/Edlin Guerra/Desktop/actividades")
library("ggplot2")
library("vegan")
library("clustsig")

# Datos del estudio publicado por: Gray JS, Clarke KR, Warwick RM, Hobbs G (1990). Detection of initial 
# effects of pollution on marine benthos: an example from the Ekofisk and Eldfisk oilfields, North Sea.  
# Marine Ecology Progress Series 66, 285-299.

# Este estudio consiste en la evaluación del macrobentos y varios contaminantes del sedimento en 39 sitios 
# dispuestos en un diseño radial alrededor de una plataforma de perforación petrolera en el mar del Norte,
# donde se espera que los contaminantes asociados a la actividad petrolera afecten la estructura del ecosistema.
# La disposición de los sitios es circular, alejándose cada ciertos kilómetros del centro de  perforación. 
# Para esta actividad ignoraremos la distancia de cada sitio respecto a la plataforma. Intentaremos clasificar 
# las muestras según sus descriptores, es decir,  dejaremos que los datos cuenten su propia historia en vez 
# de forzarlos a un modelo específico. 

# Parte 1 -----------------------------------------------------------------


#1. Análisis de clasificación o Cluster analysis. Evaluación de los métodos de agrupación

#       a) Importe la matriz de datos "sedimentos"
sedimentos <- read.csv(file.choose(),header=T, sep=",", row.names=1)
#       b) ¿Cuáles son las dimensiones del objeto?
dim(sedimentos)
#       c) ¿Cuáles son las variables que allí aparecen y cuáles deberían incluirse en el análisis?
solo las variables ambientales
#       d) Seleccione sólo las variables que ambientalmente describan la muestra de sedimento, 
#          incluyendo contaminantes. Llame a esa selección "sed".
sed<-sedimentos[,3:11]
#       e) Evalue las escalas de magnitud de las variables, así como sus promedios. ¿Qué función de R podría
#          ser útil para esto?
summary(sed)
#       f) Considerando la escala de magnitud de las variables, qué tipo de pretratamiento debe ser aplicado?
pairs(sed)

sed1<-data.frame("log.THC"=log(sed["THC"]), "log.Cu" = log(sed["Cu"]),sed[2:6],sed[8:9])
pairs(sed1)

#alternativa de código al sed1 de línea 48
library(dplyr)

sed %>% 
  mutate("log.THC" = log(THC), "log.Cu" = log(Cu)) %>% 
  select(-c(THC, Cu))

#       g) Aplique el pretratamiento y llame a la matriz resultante "sed.stand"
library(vegan)
sed.stand<-decostand(sed1, "standardize")
#       h) Aplique distancias Euclideanas a la matriz pretratada y llame a ese objeto "DE.sed"
DE.sed<-vegdist(sed.stand,"euc")
#       i) Genere un dendograma jerárquico con el método de agrupamiento simple ("single linkage"). Para esto será
#          necesario aplicar la función hclust. Le sugerimoms leer la documentación en R sobre hclust de la 
#          librearía "Stats". Por ahora le adelantamos que los argumentos necesario son la matriz de distancias y el 
#          método de vinculación, que en este caso será el simple. Llame al cluster "single.c" y grafíquelo.
single.c<-hclust(DE.sed, method = "single")
plot(single.c, main="Single linkage")
#          ¿Cuántos clusters (grupos) identifica. ¿Es capáz de reconocer algún patrón lógico?
#          ¿Cuáles son los sitios que más se parecen?¿es este gráfico evidencia de contaminación alrededor de la plataforma
#          petrolera?
dos grandes, en el primero varios grupos con casi la totalidad de las muestras
#       j) Genere un nuevo dendograma jerárquico, pero ahora con el método completo ("complete linkage").
#          Llame al cluster "comp.c" y grafíquelo. Responda: ¿cuántos grupos identifica? ¿hay algún patrón más lógico?
#          ¿cambió la clasificación respecto al método anterior?
comp.c<-hclust(DE.sed, method = "complete")
plot(comp.c, main="Complete linkage")
#       j) Genere un tercer dendograma pero ahora con el método promedio ("average linkage").
#          Llame al cluster "ave.c" y grafíquelo. Responda: ¿cuántos grupos identifica? ¿hay algún patrón más lógico?
#          ¿cambió la clasificación respecto a los dos métodos anteriores? Para tener una mejor perspectiva, puede 
#          proyectar los tres dendogramas usando la función par con una fila y tres columnas, y llamando nuevamente a 
#          los tres dendogramas. Es recomendable asignar un título a cada plot para poder hacer comparaciones efectivas.
ave.c<-hclust(DE.sed, method = "average")
plot(ave.c, main="average linkage")
par(mfrow=c(1,3),mar=c(2,4,4,2))
plot(ave.c, main="average linkage")
plot(comp.c, main="Complete linkage")
plot(single.c, main="Single linkage")
#       k) Modifique la posición de las etiquetas agregando como atributo en cada plot "hang = -0.1". ¿Prefiere esta salida o
#          la anterior?
par(mfrow=c(1,3),mar=c(2,4,4,2))
plot(ave.c, main="average linkage", hang = -0.1)
plot(comp.c, main="Complete linkage", hang = -0.1)
plot(single.c, main="Single linkage", hang = -0.1)

#       m) Hasta ahora, la identifiación de grupos es compeltamente arbitraria. Usaremos un método computacionalmente intensivo
#          para "podar" el dendograma. Esta función es la simprof de la librería {clustsig}. Copie en su cónsola ??simprof
#          para obtener ayuda de cómo ejecutar simprof. Asigne un nombre al resultado simprof e dentifique cuántos grupos realmente
#          distinguibles hay explorando ese objeto. 
library(clustsig)
sig.clust<-simprof(sed.stand, num.expected = 500, num.simulated = 99, method.cluster = "average",
                   method.distance = "euclidean",
                   method.transform = "identity", alpha = 0.05, sample.orientation =
                     "row", const = 0,
                   silent = TRUE, increment = 100)
#       p) Grafique su resultado con la función simprof.plot 
par(mfrow=c(1,1))
sig.clust.dend<-simprof.plot(sig.clust, leaflab = "perpendicular")


