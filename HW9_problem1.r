#HW 9, Problem 1

simulate_time <- function(L) {
  i <- 0
  t <- 0
  
  while (i < L) {
    
    if (i == 0) {
      rate <- 1
      t <- t + rexp(1, rate)
      i <- 1
      
    } else {
      rate <- 2
      t <- t + rexp(1, rate)
      
      if (runif(1) < 0.5) {
        i <- i + 1
      } else {
        i <- i - 1
      }
    }
  }
  
  return(t)
}

set.seed(1)

L <- 20
N <- 1000

times <- replicate(N, simulate_time(L))

mean_time <- mean(times)
var_time <- var(times)

mean_time
var_time
