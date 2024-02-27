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
colnames(tB_edad_Hijos) = c("0 hijos","1 hijo","2 hijos","3 hijos")
print(tB_edad_Hijos)

#Tabla de contingencia (Edad vs Hijos)
tB_Salario_Hijos = table(cut(dataInf$Salary, breaks = c( 60000, 70000, 80000, 90000, 100000)),dataInf$Amount.of.Children)
colnames(tB_Salario_Hijos) = c("0 hijos","1 hijo","2 hijos","3 hijos")
rownames(tB_Salario_Hijos) = c("60k+","70k+","80k+","90k+")
print(tB_Salario_Hijos)

#___________________CREACION DE GRAFICAS_________________________
colores = c("#EE3B3B","#CDAA7D","#8EE5EE","#FFF8DC")

barplot(tB_Salario_Hijos, col = colores, ylab="Edad" , xlab= "numero hijos" )

