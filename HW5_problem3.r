p=matrix(c(0.01,0.99,0,
           0.99,0,0.01,
           0,0.01,0.99),nrow=3,byrow=TRUE)

N <- 500
reps <- 10^5


p_cum <- t(apply(p, 1, cumsum))

many_chains <- matrix(0L, nrow = N, ncol = reps)

many_chains[1, ] <- 1L

for (i in 2:N) {
  
  current_states <- many_chains[i-1, ]
  
  u <- runif(reps)
  
  many_chains[i, ] <- 1L + 
    (u > p_cum[current_states, 1]) + 
    (u > p_cum[current_states, 2])
}

time <- 1:N
f_n <- rowMeans(many_chains==1)
q_n<-1/3
th_vector <- rep(1/3,times=N)

plot(x=time, y=f_n, ylim = c(min(c(f_n, q_n)), max(c(f_n, q_n))),main="Time to Converge to S.D.")
lines(x=time, y=th_vector, col="red",lwd=2)
