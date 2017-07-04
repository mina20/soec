library(Matrix)
set.seed(123)
rho<-runif(1)
data <- read.csv("TSP.csv")
distance <- dist(data[,-1])
distanceMat<-as.matrix(distance)
eta=as.matrix(1/(distanceMat))
diag(eta)<-0
pheromones<-matrix(0.5,nrow=nrow(eta),ncol=ncol(eta))
pherom<-pheromones*eta
city=c(1:nrow(data))
startcity=sample(1:nrow(data),1)
path=NULL
path=c(path,startcity)
for(i in 1:14)
{
  newcity=which(pherom[startcity,]==max(pherom[startcity,]))
  path=c(path,city[newcity])
  city=city[-startcity]
  city=city[-newcity]
  
}
  
                           
                         

