# optimization using fireflies algorithm

Booth<-function(x){

       y=(x[1]+2*x[2]-7)^2+(2*x[1]+x[2]-5)^2
      
       return(y)

}

#Generating population and Intensity
I<-NULL
mat<-NULL
for(k in 1:50){
	vec<-runif(2,-10,10)
	mat<-(rbind(mat,vec))
	B<-Booth(vec)
	I<-c(I,B)
}

b_0<-1
a<-runif(1)
g<-0.01
t<-1
maxIter<-100
while(t < maxIter){
	B1<-NULL
	for(i in 1:nrow(mat)){
		for(j in 1:i){
                	r<-dist(mat[c(i,j),]) 
                        scaleI<-I[i]-min(I)/max(I)-min(I)
                  	b<-scaleI*exp(-g*r^2)  
                  	if(I[j]>I[i]){
                  		mat[i,]=mat[i,]+b*exp(-g*r*r)*(mat[j,]-mat[i,])+a*(runif(1)-0.5)
                         }                 

         }

         B2<-Booth(mat[i,])
         B1<-c(B1,B2)
   }
	I <- B1
       rank <- order(B1) 
       print(min(B1))
      t=t+1
      plot(mat[,1],mat[,2]) 
}






