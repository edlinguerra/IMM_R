# 1. Crea una matriz con 4 filas ('nr') y 3 columnas ('nc'), con los números del 1 al 12 usando la función 'matrix' 
#    y llámala mat1.
rm(list=ls())
mat1<-matrix(1:12,nr=4,nc=3)
mat1
#    a) ¿Qué sucede si sólo defines el número de columnas?
matrix(1:12,nc=3)
#    b) ¿Qué sucede si defines el número de columnas pero adicionas el argumento 'byrow=T' en la función?
matrix(1:12,nc=3, byrow=T)

# 2. Usando subsetting selecciona la primera columna y la primera fila de mat1 de manera separada.
mat1[,1]
mat1[1,]

# 3. Ahora crea mat2 conteniendo los números del 13 al 24 y con las mismas dimensiones de mat1,
mat2<-matrix(13:24,nc=3)
#    c) ¿Como harías para sumar las dos matrices?
mat1+mat2
#    d) Adiciona el número 3 a cada elemento de la matriz mat1.
mat1+3
#    e) Resta el número 1 a mat1. 
mat1-1
#    f) ¿Como imaginas que se haría la sustracción (resta) entre mat1 y mat2?
mat1-mat2

# 4. La matriz traspuesta se calcula mediante la función 't()'. Obtén la matriz transpuesta de mat2.
t(mat2)

# 5. El operador '*' es usado para obtener el producto escalar de una matriz. 
#    a) Usando este operador estima el doble de mat1. ¿El resultado es una matriz o es un escalar?
2*mat1
#    b) ¿Qué sucede si multiplicas mat1 por mat2 uando el operador '*'? 
mat1*mat2
#    c) ¿Qué sucede si multiplicas mat1 por mat2 uando el operador '%*%'? 
mat1%*%mat2
#    d) ¿Qué sucede si multiplicas mat1 por la matriz traspuesta de mat2 usando el operador '%*%'?
mat1%*%t(mat2)
#    e) ¿Por qué tuviste que trasponer la mat2?
Porque para el producto matricial, se requiere que el número de filas de la primera matriz sea igual al número
de columnas de la segunda. 
#    f) Calcula a mano el valor que corresponde a la primera fila y primera columna del resultado de multiplicar
#       mat1 por la traspuesta de mat2.   
(1*13)+(5*17)+(9*21)
287

# 6. Para obtener la división de los elementos de dos matrices (elemento a elemento) se puede multiplicar la
#    primera matriz por una que contenga los inversos de los elementos de la segunda. Escribe el código para 
#    "dividir" la mat3 entre 2. Verifica el resultado.
mat3<-matrix(c(2,4,4,8),nc=2)
mat3*matrix(c(0.5,0.5,0.5,0.5),nc=2)

# 7. Aplica la función 'diag' a mat1 y explica que hace esta función.
diag(mat1)

# 8. La matriz inversa se calcula utilizando la función 'solve'. Aplica 'solve' a mat2.
solve(mat2)

mat3<-matrix(runif(9),nc=3)
solve(mat3)

#    a) Genera una matriz mat4 de 2 filas y 2 columnas con los elementos del 1 al 4 ordenados por columnas.
mat4<-matrix(1:4, nc=2)
#    b) Obtén la matriz inversa de mat4. ¿Cuál es el problema de mat2?
solve(mat4)
mat2 no es una matriz cuadrada y por tanto no tiene inversa; mat4 si es cuadrada.
#    c) Intenta obtener la inversa de mat3. ¿Cuál es el problema de mat3?
solve(mat3)
El determinante de mat3 es igual a cero, y su inversa no está definida.

# 9. El determinante de una matriz se calcula con la función 'det'. ¿Porque no podemos calcular el determinante de mat1?
Porque mat1 no es una matriz cuadrada 

# 10. Las siguientes instrucciones son para visualizar matrices.
#     a) Aplica la función 'plot' a mat1 y explica que hace esta función aplicada a una matriz.
plot(mat1)
#     b) Aplica la función 'image' a mat1 e intenta interpretar el resultado gráfico.
image(mat1)
Cada celda del image plot corresponde a un elemento de la matriz. Por default esta gráfica aparece sin leyendas y gira toda 
la matriz hacia la izquierda, de tal manera que el valor mat1[1,1], aparece en el extremo inferior izquierdo. La escala de 
color va de rojo a blanco, pasando por el amarillo.
#     c) El siguiente código elabora la gráfica utilizando el ggplot2. Primero transforma la matriz en un dataframe, para
#     después apilar todos los elementos en una única columna usando la función 'stack'. Después adiciona una columna 
#     al dataframe, indicando el número de celdas que se desea tener en el eje de las x (en este caso usa el número de 
#     renglones de mat1). Finalmente llama a la libreria ggplot2 y hace el gráfico con la función 'qplot'. El argumento 
#     'fill' indica la intensidad del color de cada celda de la gráfica, que correponde a los valores de los elementos 
#     de la matriz (la columna 'values'). Copia el código e identifica cada comando con una accion descrita para verificar
#     lo que hace. Los argumentos 'image', 'raster' y 'label' son para indicar el tipo de gráfico, y los valores de las
#     leyendas en cada celda.
data.frame(mat1)
stack(data.frame(mat1))
mat.s<-data.frame(stack(data.frame(mat1)),celdas=1:dim(mat1)[1])
mat.s
require(ggplot2)
qplot(data=mat.s, x=celdas, y=ind, fill=values, geom=c("raster","text"), label=values, xlab="Rows", ylab="Cols")

data(iris)

aggregate(Sepal.Length ~ Species, data = iris, mean)


