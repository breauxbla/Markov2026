#Homework 1, problem 4b
set.seed(3)

N <- 100000

X1 <- runif(N, 0, 60)
X2 <- runif(N, 0, 60)
X3 <- runif(N, 0, 60)

T <- pmax(X1, X2, X3)

hist(T,
     breaks = seq(0,60,by=1),
     freq=FALSE,
     probability = TRUE,
     col = "lightblue",
     border = "white",
     main = "Simulated Values and Theoretical Curve",
     xlab = "Minutes after 6 PM")
curve((x^2)/72000, 0, 60, add=TRUE, col="red", lwd=2)