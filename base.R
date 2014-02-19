##Metodo principal
kdensity<-function(sample,method,n=200,b=length(sample)^0.2){
  m=min(sample)-2*b
  M=max(sample)+2*b
  x.v=seq(from=m,to=M,by=(M-m)/n)
  kernel=switch(method,
                gauss=gaussian.kernel,
                triangular=triangular.kernel)
  dens.v=sapply(x.v,FUN=kernel,sample=sample,b=b)/(length(sample)*b)
  list(x=x.v,y=dens.v)
}

##FUNCIONES KERNEL
##Funcion triangular
triangular.kernel=function(x,sample,b){
  s<-sample[abs(sample-x)<=b]
  k<-0
  if (length(s)>0){
    u<-(x-s)/b
    k<-sum(1-abs(u))  
  }
  k
}
##Funcion Gaussiana
gaussian.kernel=function(x,sample,b){
  u<-(x-sample)/b
  sum((1/sqrt(2*3.14159))*exp(-0.5*u^2))
}

##FUNCIONES EXTRAS
##Calculo aproximado de integrales
numeric.integral<-function(data){
  x<-data$x
  y<-data$y
  dx<-diff(x)
  dy<-rowMeans(cbind(y[-1],y[-length(y)]))
  sum(dx*dy)
}
##Representacion grafica
plot.kernel<-function (data,col="red"){
  plot(data,main="Kernel density estimation",xlab="X",ylab="Density",col=col,type="l",lwd=2)
}



##start of the program
muestra<-rbeta(10,2,200)
muestra
triang.density<-kdensity(sample=muestra,method="triangular",n=200,b=0.005)
x11()
plot.kernel(triang.density)
numeric.integral(triang.density)
