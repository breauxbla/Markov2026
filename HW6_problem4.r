#Problem 4b
#Simulate Binary Galton-Watson Process
#a=0.49

galt_matrix <- matrix(NA,nrow=200,ncol=1000)

set.seed(125)
for (j in 1:1000){
  chain <- numeric(200)
  for(i in 2:200){
    
    chain[1]<-1
    z_i <- rbinom(n=2*chain[i-1],size=1,prob=0.51)
    chain[i] <- sum(z_i)
    
  }
  galt_matrix[,j] <- chain
}

#Calculate extinction Probability
last_row <- galt_matrix[200,]
sim_prob <- sum(last_row==0)/1000
sim_prob
