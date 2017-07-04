#simulation anneling

func<-function(x1,x2){
                  
            y= (x1+2*x2-7)^2+(2*x1+x2-5)^2
            return(y)
          }
T<-1000
step<-0.05
r<-runif(1,0,1)
eps<-0.001
for(T in 1000:1){
 
       for(i in 1:10){
        x1=runif(1,-1,1)
        x2=runif(1,4,5)
        x1_new=x1+step*runif(1)
        x2_new=x2+step*runif(1)                   
        E_old=func(x1,x2)
        E_new=func(x1_new,x2_new) 
        delE=E_old-E_new
        if(E_new<E_old || r<(-delE/T)){
         E_new=E_old
         x1=x1_new
         x2=x2_new
        }
        if(E_new-E_old<eps){
                            solution=E_new
                           }
      
        print(E_new)
  }
}
