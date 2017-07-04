n=150
k=3

n_ants=50
Q_0=0.8
nullmatrix<-matrix(rep(0,2*n),ncol=2)
x<-1:n
for(i in 1:n){
nullmatrix[i,1]=runif(1,0,25);
nullmatrix[i,2]=runif(1,0,25);
}


pheromon_mat = matrix(rep(0,(n*k)), ncol=k)
pheromon_norm_mat = matrix(rep(0,(n*k)),ncol =k)	
for(i in 1:n ) { pheromon_mat[i,] = runif(k, 0, 1)   }
grp = matrix(rep(0,(n_ants*n)),ncol = n)
fitness=NULL

for(s in 1:n_ants){
cluster_cords = matrix(rep(0,(2*k)), ncol = 2)
for(j in 1:n){
r<-runif(1,0,1)
vec1=pheromon_norm_mat[i,]
vec2=NULL
	if(r<=Q_0)
		{					
		a= sort(vec1,decreasing = T, index.return=T)$ix[1]	
		grp[h,i] =a
		}

		else			 	
		{
		r2  = runif(1,0,1); b =1
		vec2 = cumsum(vec1)
		b = (which(vec2>=r2))[1]
		grp[h,i] = b	
		}
}


for(j in 1:k){

vec = NULL
 vec = which(grp[h,]==i)	
cluster_cords[i,1] = mean(nullmatrix[vec,1]); 
cluster_cords[i,2] = mean(nullmatrix[vec,2]);
}



fit<-0
for(f in 1:n){
p = nullmatrix[v,]
		ab = grp[h,v]
		q = cluster_cords[ab,]
		fit = fit + distance(p,q)
		}
fitness[s] = fit
}


a=b=NULL;
tmp=  NULL;

a = sort(fitness, index.return = T)$ix[1]; b = sort(fitness, index.return = T)$ix[n_ants]
a = fitness[a]; b = fitness[b]
if(a!=b)		# when all the fitnesses are not same...
{
for (i in 1:n_ants )	
{
c = fitness[i];
tmp[i] = 0.2 + ((c-a)*(0.02-0.2)/(b-a))	}
}

else	{tmp = rep(0.2,n_ants)	}



for(i in 1:n_ants )
{
vec = grp[i,];
	for(j in 1:n )
	{
	a = vec[j]
	pheromon_content[j,a] = pheromon_content[j,a] + tmp[i]		# updating pheromon content...
	}
}
gr_fitness = cbind(grp,fitness)

}

print(gr_fitness)
}

distance = function(p,q)
{
dist = sqrt(((p[1]-q[1])^2)+((p[2]-q[2])^2))
return(dist)
}
