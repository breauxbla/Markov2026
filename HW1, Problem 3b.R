# Homework 1 Problem 3b

f <- function(u) {
  u^4 / (1+u^6)
}

estimated_area <- function(N) {
  u <- runif(N)
  v <- runif(N)
  
  mean(v <= f(u))
}

x <- seq(1, 5, by = 0.1)
N_vals <- floor(10^x)

E <- numeric(length(N_vals))

for(i in seq_along(N_vals)) {
  E[i] <- estimated_area(N_vals[i])
}

plot(N_vals,E,
     log="x",
     xlab="x",
     ylab="E(N) Estimates",
     main="Monte Carlo Estimates of Definite Integral")
abline(h=.14343, col="red", lwd=2)