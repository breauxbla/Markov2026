set.seed(123)
u100 <- runif(100,0,1)
u1000 <- runif(1000,0,1)
u10000 <- runif(10000,0,1)

xsample <- function(u) {
  10*(1-u)^(-1/3)
}

x100 <- xsample(u100)
x1000 <- xsample(u1000)
x10000 <- xsample(u10000)

hist(x100, 
     breaks = seq(0,60,by=1),
     freq=FALSE, probability=TRUE,
     col="lightblue", border="white",
     main="100 Simulated and Theoretical",xlab="x")
curve(3000*x^(-4), 0, 60, add=TRUE, lwd=2, col="red")

hist(x1000, 
     breaks = seq(0,119,by=1),
     freq=FALSE,
     col="lightblue", border="white",
     main="1000 Simulated and Theoretical",xlab="x", xlim=c(0,60))
curve(3000*x^(-4), 0, 60, add=TRUE, lwd=2, col="red")

hist(x10000, 
     breaks = seq(0,258,by=1),
     freq=FALSE, probability=TRUE,
     col="lightblue", border="white",
     main="10000 Simulated and Theoretical",xlab="x", xlim=c(0,100))
curve(3000*x^(-4), 0, 60, add=TRUE, lwd=2, col="red")
