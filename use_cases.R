#############################################
## USE CASE DEL MODULO INICIAL SOLO CON EL KERNEL TRIANDULAR Y GAUSS##
#############################################
#Carga del script
source("dataRead_module.R")
source("kdensity_module.R")
#Generacion de muestras aleatorias (usando una distribucion beta)
muestra<-random.beta()
muestra

#Generacion de la estimacion basada en el kernel triangular
triang.density<-kdensity(sample=muestra,method="triangular",n=200,b=0.005)
#Representacion grafica
x11()
plot.kernel(triang.density)
#Calculo del area (siempre deberia ser 1)
numeric.integral(triang.density)



##################################
#Lo mismo con el kernel gausiano
##gauss.density<-kdensity(sample=muestra,method="gauss",n=200,b=0.0025)
#Representacion grafica
##x11()
##plot.kernel(gauss.density)
#Calculo del area (siempre deberia ser 1)
##numeric.integral(gauss.density)


######################
## PRUEBAS DEL FORK ##
######################

#Cargamos el script
source("Fork.R")

#Generamos una muestra, una mazcla de dos distribuciones Gausianas, en este caso
muestra<-c(rnorm(10,-1,0.25),rnorm(25,1,0.25))
muestra

#Estas dos son las "heredadas"
triang.density<-kdensity(sample=muestra,method="triangular",n=200,b=0.5)
gauss.density<-kdensity(sample=muestra,method="gauss",n=200,b=0.25)

#Tenemos otro kernel, el de Epanechnikov
epa.density<-kdensity(sample=muestra,method="epanechnikov",n=200,b=0.5)

#Representacion grafica, el aspecto ahora es diferente
x11()
plot.kernel(triang.density)
x11()
plot.kernel(gauss.density)
x11()
plot.kernel(epa.density)

#Calculo del area (no ha cambiado en nada)
numeric.integral(triang.density)
numeric.integral(gauss.density)
numeric.integral(epa.density)

########################################
## PRUEBAS DEL SCRIPT BASE, VERSION 2 ##
########################################

# En la vesion 2 se hace algo parecido al fork, añadir un nuevo kernel

#Cargamos el script
source("Base_version_2.R")

#Generamos una muestra, una mazcla de dos distribuciones una gausiana y una beta
muestra<-c(rnorm(10,-1,0.25),rbeta(10,0.25,4))
muestra

#Estas dos son las "heredadas"
triang.density<-kdensity(sample=muestra,method="triangular",n=200,b=0.5)
gauss.density<-kdensity(sample=muestra,method="gauss",n=200,b=0.25)

#Tenemos otro kernel, el rectangular
rect.density<-kdensity(sample=muestra,method="rectangular",n=200,b=0.5)

#Representacion grafica, el aspecto ahora es diferente
x11()
plot.kernel(triang.density)
x11()
plot.kernel(gauss.density)
x11()
plot.kernel(rect.density)

#Calculo del area (no ha cambiado en nada)
numeric.integral(triang.density)
numeric.integral(gauss.density)
numeric.integral(rect.density)


###########
## MERGE ##
###########

# A la hora de hacer el merge, por un lado deberiamos tener disponibles los cuatro kernels
# Por otro lado, habría que ver que pasa con la funcion que tiene diferente implementacion en el fork y en la base
