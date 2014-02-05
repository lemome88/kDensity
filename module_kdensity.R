####functiones para el FEATURE EPANECHNIKOV

library(ggplot2) #Nueva libreria requerida

##Metodo principal
kdensity<-function(sample,method,n=200,b=length(sample)^0.2){
  #Fijar la zona en la que calcular la densidad
  m=min(sample)-2*b
  M=max(sample)+2*b
  x.v=seq(from=m,to=M,by=(M-m)/n)
  kernel=switch(method,
                gauss=gaussian.kernel,
                triangular=triangular.kernel, ##Modificada (hay que reemplazar ')' por ',')
                epanechnikov=epanechnikov.kernel)      ##Modificada (añadida)
  dens.v=sapply(x.v,FUN=kernel,sample=sample,b=b)/(length(sample)*b)
  list(x=x.v,y=dens.v)
}


#Nuevo kernel aniadido (funcion nueva)
epanechnikov.kernel<-function (x,sample,b){
  u<-(x-sample)/b
  u<-u[abs(u)<1]
  k<-0
  if (length(u)>0) k<-3/4*(1-u^2)
  sum(k)
}

#Misma función extra pero totalmente reimplementada
plot.kernel<-function(data,col="red",main="Kernel density estimation"){
  df<-data.frame(data)
  ggplot(df,aes(x=x,y=y)) + geom_line(col=col) + labs(x="X",y="Density",title=main)
}
