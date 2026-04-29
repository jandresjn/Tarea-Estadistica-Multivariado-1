#----------------------------------------------------------------------#
#                    Curso: Analisis Multivariado                      #
#      Maestría en Inteligencia Artificial y Ciencias de Datos         #
#                  Universidad Autonoma de Occidente                   #
#                        Claudia Lorena Montes                         #
#                         clmontes@uao.edu.co                          #
#                  Implementación paso a paso de ACP                   #
#----------------------------------------------------------------------#

install.packages("lessR")
# -------------------- Librerias Requeridas ------------------------####
library("readxl")

libs <- c( "readxl","openxlsx","dbplyr","dplyr",
           "lessR","FactoMineR","factoextra")
lapply(libs, library, character.only = T)

# --------------- Implementación paso a paso ------------------------#
######### PASO 1: Matriz de datos originalesDatos
getwd()
setwd("C:/Users/Claudia Montes/Desktop/UAO/Análisis Multivariado/3. Practicas/1. Practica ACP")
df=read_excel("2. Datos/IPM - 2021.xlsx", sheet = "Datos")
attach(df)
View(df)
df <- data.frame(df, row.names = 3)

### Análisis previo
### Cumplimiento de requisitos para ACP
df_numericas<-df[,-c(1,2)] ### Variables cuantitativas
summary(df_numericas) # Resumen de los datos
cor(df_numericas) ### Correlación lineal
anyNA(df_numericas); sum(is.na(df_numericas)) # Datos faltantes

######### PASO 2: Estandarización de la matriz 
# Estandarización con dbplyr
df_estandarizados <- df_numericas %>%
  mutate(across(everything(), ~ (.-mean(.))/sd(.)))
df_estandarizados

# Estandarización con la función scale
datos_estandarizados <- scale(df_numericas)
datos_estandarizados

# PASO 3: Matriz de correlaciones
M<-cor(df[,-c(1,2)]); M # Correlación directa de los datos
corR <- t(datos_estandarizados) %*% datos_estandarizados/ nrow(datos_estandarizados)  ### Z'NZ.
corR
cor(datos_estandarizados)

######### PASO 4: Encontrar los valores y vectores propios.
Descomp <- eigen(M)
Descomp
lambda<-Descomp$values ; lambda
u <- Descomp$vectors ; u
t(u[,15]) %*% u[,15]  ### u'u=1 (ortonormales)
t(u[,1]) %*% u[,15]  ### u1'u2=0 (ortogonales)

######### PASO 5: Ordenar los valores propios
lambda_ordenados <- sort(lambda, decreasing = TRUE)
lambda_ordenados
sum(lambda_ordenados)
varianza<-lambda_ordenados/sum(lambda_ordenados)*100
varianza # Varianza explicada

######### PASO 6. Construir las componentes principales.
Comp <- datos_estandarizados %*% u
Comp

######### PASO 7: Representación gráfica nube de individuos

X11()  # Abre nueva ventana gráfica
plot(
  Comp[, 1], Comp[, 2],
  col = "blue",
  xlab = paste0("PCA1 (", round(varianza[1], 2), "%)"),
  ylab = paste0("PCA2 (", round(varianza[2], 2), "%)"),
  pch = 19,
  main = "Análisis de Componentes Principales"
)
# Agregar nombres a los puntos
text(Comp[, 1], Comp[, 2], labels = rownames(df), col = "red", pos = 3)
# Agregar líneas en el origen
abline(h = 0, v = 0, col = "gray40", lwd = 1.2, lty = 2)

# PASO 7: Representación gráfica nube de variables
Z<- datos_estandarizados
n <- nrow(Z)
matrixRn <- (Z %*%t(Z))/n

DescompRn <- eigen(matrixRn)
valoresRn<-DescompRn$values
varianza_Rn<-valoresRn/sum(DescompRn$values)*100
varianza_Rn
# Comparación de valores propios Rp y Rn
Descomp$values  ### Rp
DescompRn$values  ### Rn

#### Coordenadas de las variables
v <- DescompRn$vectors

CoordRn <- (t(Z) %*% v)/ sqrt(n)
CoordRn

x11()
plot(CoordRn[,1], CoordRn[,2],xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = paste0("PCA1 (", round(varianza_Rn[1], 2), "%)"),
     ylab = paste0("PCA2 (", round(varianza_Rn[2], 2), "%)"),
  main = "Círculo de Correlaciones",
  type = "n"  # no dibuja los puntos aún
)
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lwd = 1.5, fg = "gray40") # Dibujar el círculo unitario
abline(h = 0, v = 0, lty = 2, col = "gray40") # Líneas en el origen
segments(0, 0, CoordRn[, 1], CoordRn[, 2], col = "skyblue3", lwd = 1.2) # Agregar líneas desde el origen a cada punto
points(CoordRn[,1], CoordRn[,2], col = "blue", pch = 19) # Añadir los puntos (coordenadas de las variables)
text(CoordRn[,1], CoordRn[,2], labels = rownames(CoordRn), col = "red", pos = 3) # Añadir etiquetas de las variables

#-------------------------------------------------------------------------#
#--------------------- UTILIZANDO LAS LIBRERIAS --------------------------#
#-------------------------------------------------------------------------#
resPCA <- PCA(df,quanti.sup = 1, quali.sup = 2, graph = FALSE)
resPCA
summary(resPCA)

resPCA$eig # Valores propios
resPCA$svd$U # Vectores propios
resPCA$var$coord # Componentes en Rn
resPCA$ind$coord # Componentes en Rp

x11();fviz_screeplot(resPCA) #Gráfico de porcentaje de inercia

## Representación de individuos (departamentos)
X11();fviz_pca_ind(resPCA,axes = c(1,2),geom = c("point", "text"),
                   repel = TRUE)

X11();fviz_pca_var(resPCA)## Representación de variables
X11();fviz_pca_biplot(resPCA) # Representación simultanea

### Contribución a la construcción de las componentes
X11();fviz_pca_ind(resPCA,axes = c(1,2),geom = c("point", "text"),
                   col.ind = "contrib",repel = TRUE,
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   quali.sup = NULL,col.var.sup = "blue") ## Indivuos
X11();fviz_pca_var(resPCA,col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) # Variables

x11(); fviz_pca_contrib(resPCA,choice = "var",axes = 1)
x11(); fviz_pca_contrib(resPCA,choice = "var",axes = 2)
x11(); fviz_pca_contrib(resPCA,choice = "var",axes = 1:2)
x11(); fviz_pca_contrib(resPCA,choice = "ind",axes = 1)
x11(); fviz_pca_contrib(resPCA,choice = "ind",axes = 2)
x11(); fviz_pca_contrib(resPCA,choice = "ind",axes = 1:2)

### Calidad de la representación (cosenos)

X11();fviz_pca_ind(resPCA,axes = c(1,2),geom = c("point", "text"),
                   col.ind = "cos2",repel = TRUE,
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   quali.sup = NULL,col.var.sup = "blue") ## Individuos

X11();fviz_pca_var(resPCA,col.var = "cos2",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) #Variables

x11(); fviz_cos2(resPCA,choice = "var",axes = 1)
x11(); fviz_cos2(resPCA,choice = "var",axes = 2)
x11(); fviz_cos2(resPCA,choice = "var",axes = 1:2)
x11(); fviz_cos2(resPCA,choice = "ind",axes = 1)
x11(); fviz_cos2(resPCA,choice = "ind",axes = 2)
x11(); fviz_cos2(resPCA,choice = "ind",axes = 1:2)

x11(); plotellipses(resPCA) # Agrupación

#---------------------------------------------------------------#
#---------------------- FACTOR TAMAÑO --------------------------#
#---------------------------------------------------------------#
# eliminando B_ACCESS, para visualizar el factor tamaño, si los 
# datos por si mismos no cumplen la propiedad del factor tamaño
# no se pueden obligar, este es un ejemplo para el caso que 
# lo cumplan los datos, en caso contrario no se puede usar

df_2<-df[,-c(6)] 
resPCA <- PCA(df_2,quanti.sup = 1, quali.sup = 2,graph = FALSE)
X11();fviz_pca_var(resPCA)## cumplimiento del factor tamaño
#### SI NO CUMPLE NO HACER ESTE PASO
#---------------------------------------------------------------#
#--------------------- CALCULO DE UN INDICE --------------------#
#---------------------------------------------------------------#

coor.ind<-data.frame(resPCA$ind$coord) # Selección primer componente

indice<-((coor.ind[,1]-min(coor.ind[,1]))/(max(coor.ind[,1])-min(coor.ind[,1])))*100
indice

datx<-data.frame(Departamento = rownames(df_2),
                   Coordenada_Dim1 = coor.ind[, 1],indice)
datx

# Ordenar de manera decreciente según el índice
datx <- datx[order(datx$indice, decreasing = TRUE), ]
datx

#---------------------------------------------------------------#
#-------------------------------- APP --------------------------#
#---------------------------------------------------------------#
library(Factoshiny)
PCAshiny(resPCA)



