#############################################
## USE CASE DEL MODULO INICIAL SOLO CON EL KERNEL TRIANDULAR Y GAUSS##
#############################################
#Carga del script
source("dataRead_module.R")
source("kdensity_module.R")
#Generacion de muestras aleatorias (usando una distribucion beta)
muestra<-random.beta()
muestra

epa.density<-kdensity(sample=muestra,method="epanechnikov",n=200,b=0.5)

#Representacion grafica, el aspecto ahora es diferente
x11()
plot.kernel(epa.density)

