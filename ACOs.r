data=read.csv("TSP.csv")
niter=50
ant=30
q=0.7
q0=runif(1)
distance=as.matrix(dist(data))
pheromones=matrix(1,nrow(data),nrow(data))
closeProduct=pheromones*1/distance
for(t in 1:niter){
  
    for(i in 1:ant){
 	startcity=sample(1:nrow(data),1)
	path=NULL
	        
      for(j in 1:(nrow(data)-1)){
        
	if(q<q0){
                  newcity=which(closeProduct[startcity,]==max(closeProduct[startcity,]))
                 
                  startcity=newcity
                 
                                     
                }else

		{
                 
                }
	

	}
    }
}

