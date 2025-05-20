x=runif(100,0,1)
y=runif(100,0,1)
plot(x,y,xlim=c(0,1),ylim=c(0,1))
abline(0,1) #pentru diagonala
below=y<x
points(x[below],y[below],pch=14,col='green')


#ca sa modificam conditia trebuie sa modificam below, ca sa comparam scriem sub si verficam val
#ex1a
arie1=function(n){
  x=runif(n,0,1)
  y=runif(n,0,1)
  plot(x,y)
  abline(0,1)
  below=y<x
  points(x[below],y[below],pch=14,col='yellow')
  arieestimata=length(y[below])/n
  print(arieestimata)
  
}
arie1(1000)
integrate(function(x)x,0,1)

#ex1b
arie2=function(n){
  x=runif(n,0,1)
  y=runif(n,0,1)
  plot(x,y)
  below=y<x**2
  points(x[below],y[below],pch=14,col='pink')
  arieestimata=length(y[below])/n
  print(arieestimata)
  
}
arie1(1000)
integrate(function(x)x**2,0,1)
  
#ex1c
arie3=function(n){
  x=runif(n,0,1)
  y=runif(n,0,1)
  plot(x,y)
  below=(x-0.5)**2+(y-0.5)**2>0.5**2
  points(x[below],y[below],pch=14,col='cyan')
  arieestimata=length(y[below])/n
  print(arieestimata)
  
}
ariefinal=arie3(1000)
pii=(1-ariefinal)/0.25
pii
pi

#ex1d
arie4=function(n){
  x=runif(n,0,1)
  y=runif(n,0,1)
  plot(x,y)
    below=y<1/(x+1)
  points(x[below],y[below],pch=14,col='green')
  arieestimata=length(y[below])/n
  print(arieestimata)
  
}
ariefinal=arie4(1000)
ariefinal
log(2)

#ex2 acul lui buffon
buffon=function(n){
  d=runif(n,0,1/2)
  theta=runif(n,0,pi/2)
  conditie=d<1/2*sin(theta) #conditia
  probabilitatea=mean(conditie)
  print(probabilitatea)
}

buffon(1000)
