#Problem 1: Markov HW 7

#Approximation for Part a

k_1 <- function(k){
  sapply(k, function(x){
    j <- 0:x
    1 - sum(exp(-2) * 2^j / factorial(j))
  })
}

k_2 <- function(N){
  i<-0:N

  sum(k_1(i)*((exp(-1.5)*1.5^i)/factorial(i))) 
}

k_2(100)


#Approximation and Plot for part b

b_1 <- function(k, t){
  
  lambda_A <- 2 - t/90
  lambda_B <- 1.5 - 1.5*t/90
  
  (lambda_A^k / factorial(k)) * (lambda_B^k / factorial(k)) * exp(-(lambda_A + lambda_B))
}

b_2 <- function(N,t){
  i<-0:N
  sum(b_1(i,t))
}

b_2(1000,90)

b_results <- rep(NA,90)

for (i in 1:90){
  
  b_results[i] <- b_2(100,i)
}

x<-1:90
plot(x,b_results,ylab="Probability",xlab="Time",main="Probability of a Tie (Part b)")


#Approximation and Plot for Part c
c_1 <- function(k,t){
  lambda_A <- 2/3 - t/45
  lambda_B <- 1/2 - 1.5*t/90
  
  (lambda_A^k / factorial(k))*(lambda_B^k / factorial(k))*exp(-(lambda_A+lambda_B))
}

c_2 <- function(N,t){
  i <- 1:N
  sum(c_1(i,t))
}

c_2

c_results <- rep(NA,31)

for (i in 0:30){
  
  c_results[i] <- c_2(1000,i)
}

x_2 <- 0:30
plot(x_2,c_results, main="Probability of a Tie (Part c)", ylab="Probability",xlab="Time")
