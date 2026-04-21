#HW 9, Problem 4
# Beta = 2

stoch <- function(m){
  i <- 1:(m-1)
  (1/2)*sum(1/i)
}

determ <- function(m){
  (1/2)*log(m)
}

t <- 1:50000

stoch_y <- sapply(t, stoch)
determ_y <- sapply(t, determ)

plot(t, stoch_y, type="l",
     main="Stochastic vs Deterministic Expected Time",
     xlab="m", ylab="Time")
lines(t, determ_y, col="red", lwd=2)
