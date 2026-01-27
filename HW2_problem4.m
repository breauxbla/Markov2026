# Problem 2b
F <- function(x){
  1-exp(-x)*(1+x)
}

g <- function(x,u) {
  1-exp(-x)*(1+x) - u
}

set.seed(123)
N=1000000
u <- runif(N,0,1)

X <- numeric(length(u))

system.time(for(i in seq_along(u)){
  X[i] <- uniroot(g, interval=c(0,50), u=u[i])$root
})

hist(X, freq=FALSE, breaks=100)
plot(sort(u), sort(X), main="Inverse CDF", xlab="u", ylab="x")

#Run Time 4b = 37.709

#Problem 2c
#Generate X from g(x): X~exp(1/2) -> generating u~(0,1) and plugging into -2ln(1-u)=X
#Generate u~U(0,1) and if u*cg(x)<=f(x), then return x.

X2 <- -2 * log(1-u)
u2 <- runif(N,0,1)

f <- function(x){
  x*exp(-x)
}
cg <- function(x){
  2*exp(-x/2)*exp(-1)
}

system.time(accept <- u2 <= f(X2)/(cg(X2)))

system.time(x_accept <- X2[accept])

hist(x_accept, freq=FALSE, breaks=100, main="Acceptance Rejection")

#Run time 4c = 0.01

#Problem 2D
#Generate 2*10^6 U(0,1) samples and plug into -ln(1-u) for exp(1) samples 
#Add two exp(1)'s for a single gamma(2,1) sample

N_2 <- 2000000

U_2 <- runif(N_2,0,1)

exp_one<- -log(1-U_2)

system.time(gam <- exp_one[1:N]+exp_one[N+1:N_2])

hist(gam, freq=FALSE, breaks=100, main="Sum exponential(1) method")
curve(x*exp(-x), 0,15, col="red", lwd=2, add=TRUE)

#Run time 4d = 0.032
