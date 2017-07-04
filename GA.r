#Genetic algorithm to rosenberg function
set.seed(1234)
sam <- function(x){
 
       sum <- abs(x^2-64)
          
            return(sum)
 } 

 trailSolution <- function(t1,t2){
    
           sample <-c()
           for(i in 1:10){
           sample_t <-sample(c(0,1),5,replace=T)
           sample   <-rbind(sample,sample_t)
           }
            return(sample)
       } 
trailMatrix<-trailSolution(10,5)
#trailMatrix2<-trailSolution(10,5)
   mapping <- function(trailMatrix){
        x.l<-0
        x.u<-10
          l<-5
   mapRule <-c()
           for(i in 1:10) 
           {
                
            map_code<-x.l+(((x.u-x.l)/(2^l-1))*(sum(2^((l-1):0)*trailMatrix[i,])))
            mapRule <- c(mapRule,map_code)
           }
            return(mapRule) 
         }  
      
  mapping1<- mapping(trailMatrix1)
  mapping2<- mapping(trailMatrix2)
  fittedValue<- NULL 
  for(i in 1:10){
                
                fittedFunction<-rosenBrock(mapping1[i],mapping2[i])
                fittedValue<- c(fittedValue,fittedFunction)
             }
             return (fittedValue)
matrix <- cbind(trailMatrix1,trailMatrix2)
mat <- cbind(matrix,fittedValue)
FitnessFun=function(mat,mapping,rosenBrock){
#	dataSol[sample(1:5,2),]
	r=c()
	for(i in 1:10)
		r=rbind(r,rosenBrock(mapping1,mapping2))	
	r
	}	

naturalSelection <- function(mat){
        selected <- NULL
       # for(i in 1:nrow(mat)){
         
        randomChoice <- mat[sample(1:nrow(mat),2),]
                    m<- as.matrix(randomChoice)
         #  selected <- c(selected,which(min(randomChoice)==mat[,11]))
         if(randomChoice[1,11]<randomChoice[2,11]){
            selected <- m[2,11]
          } else
            selected <- m[1,11]  
         
          #selected=as.matrix((selected),)
  }
crossoverProb<-0.6
Crossover=function(NS,CrossoverProb){
	#NewSol=NULL
	#while(nrow(NewSol)!=nrow(NS)){
	for(i in 1:nrow(NS)){
	index=sample(1:nrow(NS),2)	
	R=NULL
	R=NS[index,]
	if(runif(1)<CrossoverProb){
		cut=sample(1:(ncol(R)-1),1)
		N=rbind(c(R[1,1:cut],R[2,(cut+1):ncol(R)]),c(R[2,1:cut],R[1,(cut+1):ncol(R)]))
		NS[index,]=N
		}
	}
		NS
	}
mutation=function(crossOver,MutateProb){
	for(i in 1:nrow(crossOver)){
		for(j in 1:ncol(crossOver)){
			if(runif(1)<MutateProb)
				crossOver[i,j]<-ifelse(crossOver[i,j]==0,1,0)
			}
	}
	crossOver
	}
Sol=100
Bits=20
x=trailSolution(Sol,Bits)
x.u=10
x.l=0
x_map=mapping(trailMatrix1)
Fitness_Values=FitnessFun(mat,mapping,rosenBrock)
for(i in 1:500){
	NS=naturalSelection(x,Fitness_Values)
	crossOver=Crossover(NS,0.03)
	mutate=mutation(crossOver,MutateProb=0.2)
	x=mutate
}
x_map=mapping(trailMatrix1)
Fitness_Values=FitnessFun(mat,mapping,rosenBrock)
Fun_Optimize=min(Fitness_Values)
X_Optimize= x[which(Fitness_Values==min(Fitness_Values)),]
cat("X_Optimize:",X_Optimize,"\n")
cat("Fun_Optimize:",Fun_Optimize,"\n")
                           
                                   



                                     





