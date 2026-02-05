#Problem 4

#Part b
P_2 <- matrix(c(1/2, 1/2, 0, 0, 0, 0,
                0, 1/2, 1/2, 0, 0, 0,
                1/3, 0, 1/3, 1/3, 0, 0,
                0, 0, 0, 1/2, 1/2, 0,
                0, 0, 0, 0, 0, 1,
                0, 0, 0, 0, 1, 0), nrow=6,byrow=TRUE)

P_2power <- P_2%^%5
print(P_2power)

# Part c
set.seed(303)

states2 <- c(1, 2, 3, 4, 5, 6)
N <- 10000

rownames(P_2) <- states2
colnames(P_2) <- states2

count <- 0

for (j in 1:N){
  chain <- numeric(6)
  chain[1] <- 1
  
  for(i in 2:6){
    current <- chain[i-1]
    chain[i] <- sample(states2, size=1, prob=P_2[current,])
  }
  if (chain[6] == 4){
    count <- count+1
  } 
}

count/N

#Got About .1801 for the fraction
