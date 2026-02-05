#HW 3, Problem 1 d
a <- sqrt(3)-1
cval <- exp(-a)/(3*a^2*(1-a))

x <- seq(0,10,length=500)

f <- (1/3)*x*(1+x)*exp(-x)
g <- a^2*x*exp(-a*x)

plot(x,f,type="l",lwd=2,ylab="Density")
lines(x,cval*g,lwd=2,col="red")
legend("topright",legend=c("f(x)","c*g(x)"),
       col=c("black","red"),lwd=2)
