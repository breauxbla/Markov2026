#Problem 2f
n <- 10000

states <- c("G", "S", "D")

P <- matrix(c(9/10, 1/10, 0,
              0, 7/8, 1/8,
              2/5, 0, 3/5), nrow=3, byrow=TRUE)

rownames(P) <- states
colnames(P) <- states

set.seed(303)

chain <- character(n)
chain[1] <- "G"

for (i in 2:n){
  current <- chain[i-1]
  chain[i] <- sample(states, size=1, prob=P[current,])
}

fraction_G <- mean(chain == "G")
fraction_G
#Got about .494
