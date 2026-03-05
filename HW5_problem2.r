#Problem 2
#Prob. 2, Part b
A <- matrix(c(0.8959,0.1173,0,0,0,
              0.1041,0.7744,0.1377,0,0,
              0,0.1083,0.7495,0.1616,0,
              0,0,0.1128,0.7211,0.1896,
              0,0,0,0.1173,0.8104),nrow=5,byrow=TRUE)

eig <- eigen(A)


stat_1 <- eig$vectors[,1]

stat_dist <- stat_1 /sum(stat_1)

stat_dist

#Prob. 2, Part c
#Simulate Markov Chain 10^6 times to estimate theoretical dist
N_5 <- 10^6
p_5 <- t(A)
states_5 <- c(1,2,3,4,5)
chain_5 <- numeric(N)
chain_5[1]<-1

for (i in 2:N_5){
  current <- chain_5[i-1]
  chain_5[i] <- sample(states_5, size=1,prob=p_5[current,]) 
  
}

hist(chain_5, probability=TRUE, main="Simulated S.D.",xlab="States")


#Prob.2, Part d
#Create function to find value of Theoretical S.D.
pi_k <- function(k){
  num <- exp(-0.06*k*(k-1))
  
  vec_pi<-numeric(5)
  for (i in 1:5){
    vec_pi[i] <- exp(-0.06*i*(i-1))
  }
  denom <- sum(vec_pi)
  return(num/denom)
}

theory_dist <- numeric(5)

for (i in 1:5){
  theory_dist[i] <- pi_k(i)
}
theory_dist

#Create Histogram for Theoretical S.D.
reps_theory <- c(theory_dist[1]*N_5,theory_dist[2]*N_5,theory_dist[3]*N_5,
          theory_dist[4]*N_5,theory_dist[5]*N_5)
statdist_theory <- rep(c(1,2,3,4,5),times=reps_theory)

hist(statdist_theory, probability=TRUE, main="Theoretical Result", xlab="States")

#Create Histogram for Numerical S.D.
reps <- c(296000,263000,207000,144000,89000)
statdist_v <- rep(c(1,2,3,4,5),times=reps)

hist(statdist_v, probability=TRUE, main="Numerical Result", xlab="States")
