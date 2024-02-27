install.packages("readxl")
install.packages(c("readxl", "Mass", "dplyr"))

library(readxl)
library(MASS, lib.loc = "C:/Program Files/R/R-4.3.2/library")
library(dplyr)
library(readxl)

dataInf = read.csv("DataBase.csv", header = T, sep = ",")

Tb= as.data.frame(table(dataInf$Age))

#Creamos una variable con lungitud de datos
n = length(dataInf$Name)

#Renombramos Columnas
colnames(Tb)[1]="Edad"
colnames(Tb)[2]="fi"

Tb$Edad =  as.numeric(Tb$Edad)

#Creamos la tabla de frecuencias 
Tb$ri=Tb$fi/n
Tb$Fi=cumsum(Tb$fi)
Tb$Ri=cumsum(Tb$ri)

#Creacion de variable con datos de edad
valNew = dataInf$Age

#Creacion de Histogramas adicionalmente definimos los limites con la funcion c() que nos ayuda a definir
#vectores como rango en este caso
hist(valNew, col = "#BF3EFF", main = "Frecuencia de edades ",xlab = "Edad", ylab = "Frecuencia", ylim =c(0,20), xlim = c(20,50))


#Generamos Tabla de contingencia para comparacion de valores en diferentes casos, para el analisis del problema (Resolucion del objetivo)

#____________________________CREACION DE TABLAS DE CONTINGENCIA____________________________________

#Tabla de contingencia (Edad Vs Salario)
tB_edad_Sal = table(cut(dataInf$Age, breaks = c(20,25,30,35,40,45)) ,cut(dataInf$Salary, breaks = c( 60000, 70000, 80000, 90000, 100000)))
colnames(tB_edad_Sal) = c("60k+","70k+","80k+","90k+")   
print(tB_edad_Sal)

#Tabla de contingencia (Edad vs Hijos)
tB_edad_Hijos = table(cut(dataInf$Age, breaks=c(20,25,30,35,40,45) ),dataInf$Amount.of.Children)
colnames(tB_edad_Hijos) = c("0","1","2","3")
print(tB_edad_Hijos)

#Tabla de contingencia (Edad vs Hijos)
tB_Salario_Hijos = table(cut(dataInf$Salary, breaks = c( 60000, 70000, 80000, 90000, 100000)),dataInf$Amount.of.Children)
colnames(tB_Salario_Hijos) = c("0","1","2","3")
rownames(tB_Salario_Hijos) = c("60k+","70k+","80k+","90k+")
print(tB_Salario_Hijos)

#_________________________CREACION DE GRAFICAS_________________________#

#Usaremos mosaic plot porque es la mejor forma de visualizar los datos, para el analisis que estamos realizando

#Configuramos variables para colores
coloresT1 = c("#548B54","#7CCD7C","#90EE90","#9AFF9A")
coloresT2 = c("#00688B","#009ACD","#00B2EE","#00BFFF")
coloresT3 = c("#8B3A3A","#CD5555","#EE6363","#FF6A6A")

#GRAFICA EDAD VS SALARIO
mosaicplot(tB_edad_Sal, col = coloresT1, ylab="Salario" , xlab= "Rangos de edad" , cex= 1, main="Salario segun edad", margin=0.1)

#GRAFICA EDAD VS HIJOS
mosaicplot(tB_edad_Hijos, col = coloresT2, ylab="Hijos" , xlab= "Rangos de edad" , cex= 1, main="Hijos segun edad", margin=0.1)

#GRAFICA SALARIO VS HIJOS
mosaicplot(tB_Salario_Hijos, col = coloresT3, ylab="Hijos" , xlab= "Salario" , cex= 1, main="Hijos segun salario", margin=0.1)


