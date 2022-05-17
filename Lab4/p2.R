# 2. Métodos de interpretación: Heat Maps para avariables biológicas

#       a) Importe la matriz de datos "macrofauna.cvs" y renómbrela "matriz.macrofauna"
matriz.macrofauna <- read.csv(file.choose(),header=T, sep=",", row.names=1)
#       b) Evalue el contenido de la matriz: unidades, escalas y dimensiones.
matriz.macrofauna
dim(matriz.macrofauna)

#       c) Genere una nueva matriz pero filtrando las primeras dos columnas. Llámelas "macrofauna"
macrofauna<-matriz.macrofauna[,2:174]
#       d) Calcule descriptores univariados por cada sitio, use la riqueza de especies, la abundancia de individuos y el índice de 
#          diversidad de Simpson. Para ello genere un data.frame con las columnas como las variables, y filas como los sitios.
#          Llame a la tabla generada "uni". Trate de identificar patrones de variación en esto sestimadores según el sitio
uni<-data.frame("S" = specnumber(macrofauna),
                "N" = apply(macrofauna, 1, sum),
                "Simpson" = diversity(macrofauna, "simpson"))
uni
#       e) Para usar una aproximación multivariada, aplique el índice de similitud Bray-Curtis luego de transformas las abundancias
#          a raíz cuarta. Llame a la matriz de abundancias transformadas "r.c" y a la matriz Bray-Curtis resultante "bray1".
#          Trate de identifcar un patrón de asociación entre las muestras viendo a "bray1".
r.c<-macrofauna^0.5
bray1<-vegdist(r.c, method="bray")
length(bray1)
#       f) Genere un dendograma jerárquico con el método de agrupamiento "average linkage" o  UPGMA. Para esto será
#          necesario aplicar la función hclust. Llame al cluster "cluster.bray" y conviértalo en dendograma con nuevo 
#          nombre "dend.macro". Encuentra familar esa representacion? compárela con el dendograma ave.c de las variables ambientales
#          en un solo plot. Identifique un patrón respecto a grupos y muestras que lo conforman. ¿Hay correspondencia en ambos análisis? 
cluster.bray<-hclust(bray1, method = "average")
dend.macro<-as.dendrogram(cluster.bray)
plot(dend.macro, main= "Macrofauna, Bray-Curtis 4rt")
par(mfrow=c(1,2))
plot(dend.macro, main= "Macrofauna, Bray-Curtis 4rt")
plot(ave.c, main= "Sedimentos, Dist. Euclideanas", hang=-0.1)
par(mfrow=c(1,1))

#       g) Identifique la relación entre las matrices con la función Mantel. Esto nos dará una idea de "cuan" similares son las relaciones 
#          espaciales según sus descriptores. En teoría, si los contaminantes afectan el macrobentos, debe haber correlación entre las matrices.
#          Ejecute la prueba Mantel usndo el coeficiente de correlación Spearman y construya una hipótesis nula con 9999 permutaciones.
mantel(bray1, DE.sed, method="spearman", permutations=9999)
#       h) ¿Cuán importante cree usted que es la similitud entre ambas matrices? Sirve esta aproximación para asociar matrices de igual dimensión?
#       i) Ahora apliquemos la prueba estadística "simprof" para podar el dendograma del macrobentos usando un método cuantitativo.
#          ¿Cuántos grupos estadístcamente significativos identifica el análisis jerárquico usando el método promedio?

sig.clust.macro <-
  simprof(
    r.c,
    num.expected = 1000,
    num.simulated = 999,
    method.cluster = "average",
    method.distance = "braycurtis",
    method.transform = "identity",
    alpha = 0.05,
    sample.orientation = "row",
    const = 0,
    silent = TRUE,
    increment = 100
  )

sig.clust.dend.m <- simprof.plot(sig.clust.macro, leaflab = "perpendicular")

#       j) Utilicemos ahora la función "heatmap" para apreciar la contribución de las especies a la estructuración de los grupos.
#          Primero, como práctica, generemos un factor, con base en el grupo "distancia" en la matriz.macrofana.
distancia<-as.factor(matriz.macrofauna[,1])
#       k) Identifique cuáles son los niveles del factor distancia generado llamando a ese objeto
distancia
#       m) Lea en el menú de ayuda sobre la función "heatmap". Esta función requiere definir varios argumentos, los más importantes son:
#          la matríz con los valores de abundancia de especies y el dendograma final. Básicamente, la función reordena las filas en la matriz
#          de abundancias, y asigna un color a las abundancias, que aumenta según la intensidad. Las columnas las asocia con base en un dendograma
#          basado en correlacioens entre las especies. Luego genera un gráfico que facilita interpretar las especies que generan los grupos.
#          Lo primero que debe hacer es convertir el dataframe "r.c" a una matriz de nombre "r.c.m"
r.c.m<-as.matrix(r.c)
#          Luego debe llamar a la función "heatmap", incluyendo la matrix r.c.m y al dendograma "dend.macro" 
heatmap(r.c.m, Rowv = dend.macro, labRow = distancia)
#       n) ¿Puede interpretar algo del HeatMap generado?
#       l) Mejoresmos el heatmap incluyendo solo a las 10 especies que mejor contribuyen a diferenciar los grupos en las distancias
#          1 y 4. Para esto usaremos la herramienta SIMPER desarrollada por Clarke (1993), original del software PRIMER, pero incluída en la librearía
#          Vegan. Esta rutina descompone las similitudes entres cada par de sitios y relativiza el peso de cada especie en contribuir a la 
#          disimilitud promedio entre dos grupos. Para más información:
??simper
#          Apliquemos "simper" a la matriz "r.c" usando el factor distancia. PAra poder acceder al resultado, llamaremos a este "simper_1v4"
#          y al resumen de los resultados sum1v4
simper_1v4<-simper(r.c, distancia)
sum1v4<-summary(simper_1v4)
#       o) El objeto "sum1v4" es una lista, y como tal, podemos observar su elementos uno a uno. Identifiquemos las especies que mayor
#          contribución a las diferencias entre los grupos 1 y 4 con el siguiente comando
sum1v4$`1_4`
#       p) Generemos un vector de nombre "sp" con los nombres de las 10 primeras especies y usémoslo como subsetting en la matriz "r.c", que simultáneamente
#          podemos convertir de data.frame a matriz con el nombre "r.c.sub"
sp <- rownames(as.data.frame(sum1v4$`1_4`[1:20,]))
r.c.sub<-as.matrix(r.c[,sp])
#       q) Genere un nuevo heatmap, pero usando la matriz de 10 especies. ¿Qué logra apreciar?
heatmap(r.c.sub, Rowv = distancia, labRow = distancia)

## Reordenando columnas por similitud entre especies con índice de Whittaker

total <- apply(macrofauna, MARGIN = 2, FUN = sum)
mac2 <- macrofauna
for (i in 1:nrow(macrofauna)){
  mac2[i,] <- 100*(macrofauna[i,]/total)  
}

d <- vegdist(t(mac2[,sp]), method = "bray")
cluster.sp <- hclust(d, method = "average")
dend.sp<-as.dendrogram(cluster.sp)
plot(dend.sp, main= "Similitud entre especies, Whittaker's index of association")

heatmap(r.c.sub, Rowv = distancia, labRow = distancia, Colv = dend.sp)
heatmap(r.c.sub, Rowv = distancia, Colv = NULL, labRow = distancia)

