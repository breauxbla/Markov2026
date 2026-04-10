#HW 5 Problem 2

#Part c

#Sample times for Team A
#x=-ln(1-u)/3 is ~exp(3) and is the time of arrival for scores
#check every minute, if a point was scored: x<= 1
set.seed(333)
arrivals_A <- c()
current_time <- 0

while(current_time < 48){
  u<-runif(1,0,1)
  x <- -1/3*log(1-u)
  
  current_time <- current_time+x
  
  if(current_time<=48){
    arrivals_A <- c(arrivals_A,current_time)
  }
}

#Arrival Times for team B
set.seed(334)
arrivals_B <- c()
current_time <- 0

while(current_time < 48){
  u<-runif(1,0,1)
  x <- -1/3*log(1-u)
  
  current_time <- current_time+x
  
  if(current_time<=48){
    arrivals_B <- c(arrivals_B,current_time)
  }
}

plot(arrivals_A,rep(0,length(arrivals_A)),xlim=c(0,48),ylab="",xlab="Arrival Times A (Red) 
     and B (Blue)")
abline(v=arrivals_A, col="red",lwd=1)
points(arrivals_B, rep(1,length(arrivals_B)))
abline(v=arrivals_B, col="blue",lwd=1)


#Part d:Each team scores basket with prob. 1/2

set.seed(123)
arrivals_A2 <- c()
arrivals_B2 <- c()
current_time <- 0

while(current_time<48){
  u <- runif(1,0,1)
  x <- -1/3*log(1-u)
  
  current_time <- current_time+x
  
  if(current_time<48){
    u_new <- runif(1,0,1)
    
    if(u_new<=1/2){
      arrivals_A2 <- c(arrivals_A2,current_time)
      
    }else{
      arrivals_B2 <- c(arrivals_B2, current_time)
    }
    }
}

plot(x=arrivals_A2,y=rep(0,length(arrivals_A2)),xlab="Single Poisson Process Game",ylab="")
abline(v=arrivals_A2,col="red",lwd=1)
points(arrivals_B2,y=rep(1,length(arrivals_B2)))
abline(v=arrivals_B2,col="blue",lwd=1)

#Part e: Efficiently simulation 10^5 games

results_e <- rep(NA,100000)

for (i in 1:100000){
  N <- rpois(1,288)
  N_A <- rbinom(1,N,1/2)
  N_B <- N-N_A
  results_e[i] <- 2*(N_A-N_B)
}

#E[D(t)] Simulated:approx=0.0725, theory=0
sum(results_e)/100000

#Var(D(t)) Simualated:approx=1142.245, theory=1152
var(results_e)

#P(D(t)=0) Simulated=0.02305, theory=0.0235
sum(results_e==0)/100000
