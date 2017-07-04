

feature_selection<- function()
{
p_m = 0.05; p_c = 0.8;
library(e1071)
mat= read.csv("data.csv");
size = dim(mat)
nrow = size[1];ncol= size[2]; 
data_mat= mat[,1:(ncol-1)]

class = mat[,ncol]
no_of_features = (ncol -1) ;  no_of_chromosomes= 20
feature_set = matrix(ncol = no_of_features, nrow =no_of_chromosomes);
	for(i in 1:no_of_chromosomes )
	{
		for(j in 1:no_of_features)
		{	feature_set[i,j] = round(runif(1,0,1))	}
	}



for(h in 1:25 )
{	
fitness = NULL
	for (i in 1:no_of_chromosomes )
	{
		vec = NULL	
		vec= which(feature_set[i,]==1)
		selected_features = data_mat[,vec]
		svm_output= svm(selected_features,class,cross = 10,type ='C')
		fitness[i] = svm_output$tot.accuracy - (length(vec)/no_of_features)
	}	
features_fitness = cbind(feature_set,fitness)

fit = sort(fitness,decreasing=T, index.return=T)$ix
best_fit1 = feature_set[fit[1],]
best_fit2 = feature_set[fit[2],]
vector1 =NULL
vector1 = which(feature_set[fit[1],]==1)
cat("iteration No.:", h, "\n")
cat("vector of indices of best fitted features selected\n", vec,"\n")
cat("fitness for this set of features\n",fitness[fit[1]],"\n")

r1 = runif(1,0,1)
if(r1<p_c)
{
x = 1:no_of_features;
p = sample(x,1); x = x[-p];
q = sample(x,1);
mn =  min(p,q); mx = max(p,q);

	x = best_fit1[mn:mx]  
	best_fit1[mn:mx] = best_fit2[mn:mx];
	best_fit2[mn:mx] = x;

}
##################################################################################
# Mutation 
for(i in 1:no_of_features )
{	
r  = runif(1,0,1)
	if(r<p_m)
	{
		if(best_fit1[i]==0)
		{	best_fit1[i] = 1		}
		else
		{	best_fit1[i] = 0		}

	}
r = runif(1,0,1)
	if(r<p_m)
	{
		if(best_fit2[i]==0)
		{	best_fit2[i] = 1		}
		else
		{	best_fit2[i] = 0		}

	}
}
l_fit = sort(fitness,decreasing =F, index.return=T)$ix
feature_set[l_fit[1],] = best_fit1
feature_set[l_fit[2],] = best_fit2



}



}

