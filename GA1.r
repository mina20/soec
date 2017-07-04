#Genetic algorithm to rosenberg function
#set.seed(1234)
sam <- function(x)
     {
 
       sum <- abs(x^2-64)
          
            return(sum)
     } 
pop<-50
gen<-100
bits<-5
sampleb <-c()
           for(i in 1:pop)
          {
           sample_t <-sample(c(0,1),bits,replace=T)
           sampleb   <-rbind(sampleb,sample_t)
          }
binTodec<-function(x){
         return (sum (rev(x)*2^( rev (seq_along(x))-1)))
         }

m <- NULL
for(j in 1:gen)
{
Fitness<-NULL

	for(i in 1:nrow(sampleb)){
        FittedValue<-sam(binTodec(sampleb[i,])) 
        Fitness<-c(Fitness,FittedValue)
        }
Fit<-cbind(sampleb,Fitness)
NatSel<-NULL
        #NatSel<-NULL
           
      # randomChoice<-replicate(10,Fit[sample(1:nrow(Fit),2),])
         for(i in 1:pop){
           ind<-sample(1:nrow(Fit),2)
          temMat<-Fit[ind,]
           index<-order(temMat[,ncol(temMat)])
           choose<-index[1] 
         #select<-(min(randomChoice[,ncol(Fit),i]))
         #index<-which(randomChoice[]) 
         #NatSel<-c(NatSel,select)
         NatSel<-rbind(NatSel,Fit[choose,])
        }
CrossProb<-0.6
        for(i in 1:nrow(NatSel)){
        if(runif(1)< CrossProb)
	index=sample(1:nrow(NatSel),2)	
	R=NULL
	R=NatSel[index,]
	if(runif(1)<CrossProb)
		{
		cut=sample(1:(ncol(R)-1),1)
		N=rbind(c(R[1,1:cut],R[2,(cut+1):ncol(R)]),c(R[2,1:cut],R[1,(cut+1):ncol(R)]))
		N1<-NatSel[index,]
               # NatSel<-N[,-ncol(N1)]
		}

             NatSel
             NS<-NatSel[,-ncol(N1)]
	}
mutProb<-0.01

        
          for(i in 1:nrow(NS))
           {
             if(runif(1)<mutProb)
             {
               ind<-sample(1:ncol(NS),1)
               NS[i,ind]<-ifelse(NS[i,ind]==0,1,0) 

              }
              
             mut<-NS
		
            }
sampleb<-mut
Fitn<-NULL

	for(i in 1:nrow(mut))
	{
        sampl<-binTodec(mut[i,])
        FValue<-sam(sampl)
        Fitn<-c(Fitn,FValue)
        #mat<-cbind(Fitn,sampl)
	
        }
	m[j]<-(min(Fitn))
	print(m)
#Fit_<-cbind(sampl,Fitn)

}
print((min(m)))




