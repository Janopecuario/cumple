library(tidyverse)
library(randomNames)
set.seed(47)
cumple <- function() {
  contador<-0
  coincidencia=FALSE
  cumples<-data.frame(nombre=as.character(),dia=integer())
  while(coincidencia==FALSE){
    cumples[contador+1,1]<-randomNames(n=1)
    cumples[contador+1,2]<-sample(1:365,size=1)
    coincidencia<-anyDuplicated(cumples$dia)
    contador<-contador+1
  }
  return(contador)
}

simulacion<-data.frame(nsim=integer(),eventos=integer())
for (i in 1:1000){
  simulacion[i,1]<-i
  simulacion[i,2]<-cumple()
  print(simulacion[i,2])
}
ggplot(simulacion,aes(x=eventos))+geom_density()+theme_minimal()+
  xlab("Número de gente")+ylab("Densidad de frecuencia")
