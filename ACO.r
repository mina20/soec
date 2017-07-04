#ant colony optimization for travalling salesman problem
library(Matrix)
set.seed(123)
rho<-runif(1)
data <- read.csv("TSP.csv")
distance <- dist(data[,-1])
distanceMat<-as.matrix(distance)
eta=as.matrix(1/(distanceMat))
diag(eta)<-0
pheromones<-matrix(0.5,nrow=nrow(eta),ncol=ncol(eta))
path<-NULL
for(i in 1:20)
{
closenessProduct<-pheromones*eta
city<-sample(1:nrow(data))
startCity<-city[1]
for(j in 1:14)
  {
      q0 <- 0.7
      r <- runif(1,0,1)
      if(r < q0){
                   #print("Exploit")
                   newCity[j]<-which(closenessProduct[j,]==max(closenessProduct[j,]),arr.ind=T)
                   startCity<-newCity[j]
                   path<-c(startCity,newCity)
                 }
                  else
                 {
                   #print("Explore")
                   pheromonesMat<-matrix(runif(14*14), ncol=14)
                   newCity<-which(pheromonesMat[j,]==max(pheromonesMat[j,]),arr.ind=T)
                   startCity<-newCity[j]
                   path<-c(startCity,newCity[j])  
                 }
                  print(path)
                  best_path=()  
   }
}
 for(i in 1:20){
 pheromones<-matrix(0.5,nrow=nrow(eta),ncol=ncol(eta))
 Pheromones_update<-(1-rho)*which(pheromones==min(diatanceMat))
                
}
SUM<-0
for(l in 1:(length(path)-1))
  {
    SUM <- SUM + distanceMat[path[l],path[l+1]]
  }
 # ant_travel_dist[i] <- SUM 








