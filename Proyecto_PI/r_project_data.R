install.packages("readxl")
install.packages(c("readxl", "Mass", "dplyr"))

library(readxl)
library(MASS, lib.loc = "C:/Program Files/R/R-4.3.2/library")
library(dplyr)
library(readxl)

dataInf = read.csv("DataBase.csv", header = T, sep = ",")

dF = as.data.frame(table(dataInf$Age, dataInf$Salary ,dataInf$Amount.of.Children))



s