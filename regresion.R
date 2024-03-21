
nuevo_dir<- "C:\regresion"

---
title: "Regresión lineal simple"
author: "Agustín Romero Reina"
date: "2024-03-16"
output: html_document
---
  

#1
A través del análisis y la interpretación de artefactos, estructuras, restos humanos y otros vestigios arqueológicos, podemos obtener información valiosa sobre cómo vivían, trabajaban, se relacionaban y se organizaban las sociedades antiguas. Sin embargo, es importante tener en cuenta que la interpretación de estos restos no es siempre directa o inequívoca. Requiere un enfoque cuidadoso y multidisciplinario que incorpore no solo la arqueología, sino también disciplinas como la antropología, la geología, la biología y la historia. Además, es esencial contextualizar los hallazgos dentro de su entorno arqueológico más amplio y considerar el contexto cultural, social y ambiental en el que se produjeron. 

#2
No, el análisis de correlación lineal de Pearson no establece ningún tipo de relación causa-efecto entre variables. Se utiliza para medir la fuerza y la dirección de la relación lineal entre dos variables continuas. Mientras que una correlación positiva indica que a medida que una variable aumenta, la otra también tiende a aumentar, y viceversa, una correlación negativa indica que a medida que una variable aumenta, la otra tiende a disminuir. Sin embargo, la correlación lineal de Pearson no indica que un cambio en una variable causa un cambio en la otra variable.

#3
La causalidad se refiere a la relación en la que un evento, denominado causa, produce otro evento, denominado efecto. En otras palabras, implica que un cambio en una variable (la causa) produce un cambio en otra variable (el efecto). La causalidad implica una relación de determinación o influencia directa entre dos o más fenómenos.

Por ejemplo, consideremos el efecto del ejercicio físico en la salud cardiovascular. Si realizamos un estudio en el que un grupo de personas sigue un programa regular de ejercicio mientras que otro grupo no lo hace, y luego comparamos sus niveles de salud cardiovascular, podríamos observar que aquellos que participaron en el programa de ejercicio tienen una mejor salud cardiovascular en comparación con el grupo que no lo hizo. En este caso, se puede argumentar que el ejercicio físico (causa) está causando una mejora en la salud cardiovascular (efecto). Sin embargo, es importante tener en cuenta que la causalidad puede ser compleja y puede estar influenciada por múltiples variables y factores que deben ser controlados en un estudio experimental.

#4
Los parámetros involucrados son la pendiente, la ordenada en el origen o intercepto, el error, las variables independientes y las variables dependientes.

#5
No, no estás en lo correcto. En un plano cartesiano, el eje horizontal se denomina eje de abscisas o eje x, mientras que el eje vertical se denomina eje de ordenadas o eje y. El eje de ordenadas (y) representa las coordenadas verticales, mientras que el eje de abscisas (o eje x) representa las coordenadas horizontales.

#6

La recta de regresión se utiliza en el contexto de la regresión lineal simple, donde hay una sola variable independiente y una variable dependiente. Es una línea recta que mejor se ajusta a los puntos de datos en un diagrama de dispersión. Por otro lado, el plano de regresión se utiliza en la regresión lineal múltiple, donde hay dos o más variables independientes y una variable dependiente. Es un plano en un espacio tridimensional que mejor se ajusta a los puntos de datos en dicho espacio.

#7
Son: linealidad, homocedasticidad, normalidad e independencia.

#8

```{r, echo=TRUE}

distancia <- c( 1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)
cuentas <- c(110,2,6,98,40,94,31,5,8,10)

datos <- data.frame(distancia, cuentas)

recta <- lm(cuentas ~ distancia, data = datos)
summary(recta)
```
#9
Estos coeficientes de la ecuación de regresión indican la pendiente (-1.0872) e intercepto (95.3710), los cuales explican cómo cambia la variable dependiente a medida que varía la variable independiente.

#10
Un intercepto o intersección con un valor de 0 implica que cuando la variable independiente es 0, la variable dependiente también es 0.

#11
El análisis de regresión lineal lleva a cabo una ponderación utilizando el método de mínimos cuadrados ordinarios (MCO) para calcular los valores de los parámetros que configuran la recta de regresión. Este método minimiza la suma de los cuadrados de las diferencias entre los valores observados y los valores predichos por el modelo de regresión. Es decir, busca encontrar los coeficientes que minimizan la suma de los cuadrados de los residuos (diferencias entre los valores observados y los valores predichos), lo cual garantiza que la recta de regresión ajustada se acerque lo más posible a los datos observados.

#12
```{r, echo=TRUE}

distancia_n <- 1.1
prediccion <- predict(recta, newdata = data.frame(distancia = distancia_n))
print(prediccion)

prediccion_err <- predict(recta, newdata = data.frame(distancia = distancia_n), se.fit = TRUE)$se.fit
print(prediccion_err)
```
#13
```{r, echo=TRUE}
cuentas_predict <- c(6, 98, 40, 94, 31, 5, 8, 10)
predicciones_cuentas <- c(-6.682842, 85.520196, 28.938591, 84.216973, 53.69983, 19.924631, 28.504183, -2.121561)
residuos <- cuentas_predict - predicciones_cuentas
print(residuos)
```
#14

```{r, echo=TRUE}
shapiro.test(residuos)

#15
```{r, echo=TRUE}
set.seed(123) 
indices_training <- sample(1:length(cuentas), 0.7 * length(cuentas)) 
datos_training <- datos[indices_training, ]
datos_test <- datos[-indices_training, ]
```
#16
```{r, echo=TRUE}
library(caret)
control <- trainControl(method = "cv", number = 5)
modelo_cv <- train(cuentas ~ ., data = datos, method = "lm", trControl = control)
print(modelo_cv)
```
#17

Cuando se calculan los coeficientes de regresión con un intervalo de confianza del 95%, estamos diciendo que tenemos un 95% de confianza en que los valores reales de los coeficientes se encuentran dentro de ese intervalo. Esto significa que hay un 5% de probabilidad de que los coeficientes estimados estén fuera de ese intervalo debido al azar.

Si el nivel de significancia es 0.01, lo que indica que estamos dispuestos a cometer un error de tipo I de solo el 1%, entonces necesitamos ser más seguros en nuestras estimaciones. Por lo tanto, calculamos los coeficientes de regresión con un intervalo de confianza del 99%, lo que significa que tenemos un 99% de confianza en que los valores reales de los coeficientes se encuentran dentro de ese intervalo. Esto deja solo un 1% de probabilidad de que los coeficientes estimados estén fuera del intervalo debido al azar. En otras palabras, estamos siendo más conservadores en nuestras conclusiones al aumentar el nivel de confianza del intervalo.

#18
Si las estimaciones en un modelo lineal exhiben menor precisión en un rango específico de valores, sugiere la presencia de heterocedasticidad. En contraste, la homocedasticidad se da cuando la varianza de errores permanece constante a lo largo de todo el rango de valores

#19
La medida de precisión estadística que nos indica el porcentaje de variabilidad explicada de la variable dependiente por nuestro modelo lineal es el coeficiente de determinación, también conocido como R cuadrado (R^2). Este valor varía entre 0 y 1, y representa la proporción de la varianza total de la variable dependiente que es explicada por el modelo de regresión. Un valor de R^2 cercano a 1 indica que el modelo explica una gran parte de la variabilidad de la variable dependiente, lo que sugiere un buen ajuste del modelo a los datos. Por otro lado, un valor de R^2 cercano a 0 indica que el modelo no explica bien la variabilidad de la variable dependiente y puede sugerir que el modelo no es adecuado para los datos.

#20
Una observación atípica, también conocida como valor atípico o outlier, es un punto de datos que se aleja significativamente del resto de los datos en un conjunto de datos. Estos valores pueden ser el resultado de errores de medición, errores de entrada de datos o eventos raros. Los valores atípicos pueden influir en las estimaciones de los parámetros del modelo y distorsionar las conclusiones derivadas del análisis. Por otro lado, una observación que produce "apalancamiento" del modelo se refiere a una observación que tiene un valor extremo en una o más variables independientes, lo cual significa que la observación está ubicada en una región del espacio de las variables independientes donde hay pocos o ningún otro dato. Las observaciones con alto apalancamiento pueden ejercer una gran influencia en la estimación de los parámetros del modelo, especialmente si están lejos del centro de los datos en términos de las variables independientes, esto puede llevar a una alta sensibilidad del modelo a estas observaciones y a una influencia desproporcionada en la forma de la línea de regresión.









