#Markov HW 8, Problem 1

#Simulate Continuous Time Markov Chain

#Simulate 100 Independent Markov Chains
N <- 100
t_seq <- seq(0, 5, length.out = 150)

state_matrix <- matrix(NA, nrow = N, ncol = length(t_seq))

set.seed(42)

for (i in 1:N) {

  if (runif(1) < 1/3) {
    curr_state <- 1
  } else {
    curr_state <- 2
  }
  
  curr_time <- 0

  time_to_jump <- rexp(1, rate = 1)
  
  for (j in 1:length(t_seq)) {
    target_time <- t_seq[j]
    
  
    while (curr_time + time_to_jump <= target_time) {
      curr_time <- curr_time + time_to_jump    
      curr_state <- (curr_state %% 4) + 1      
      time_to_jump <- rexp(1, rate = 1)         
    }
    state_matrix[i, j] <- curr_state
  }
}
f_t <- colMeans(state_matrix == 1)

results_100 <- data.frame(t_seq,f_t)

theory_func <- function(t){
  1/4 - 1/12*exp(-2*t) + exp(-t)*((1/6)*cos(t)-(1/3)*sin(t))
}

x_100 <- 0:150
y_100 <- theory_func(x_100)


plot(x=results_100$t_seq,y=results_100$f_t,main="100 Ind. Markov Chains",
     ylab="y_1(t)",xlab="Time")
lines(x=x_100, y=y_100,col="red",lwd=2)


#Simulating 1000 Independent Markov Chains
N <- 1000
t_seq <- seq(0, 5, length.out = 150)

state_matrix <- matrix(NA, nrow = N, ncol = length(t_seq))

set.seed(43)

for (i in 1:N) {
  
  if (runif(1) < 1/3) {
    curr_state <- 1
  } else {
    curr_state <- 2
  }
  
  curr_time <- 0
  
  time_to_jump <- rexp(1, rate = 1)
  
  for (j in 1:length(t_seq)) {
    target_time <- t_seq[j]
    
    
    while (curr_time + time_to_jump <= target_time) {
      curr_time <- curr_time + time_to_jump    
      curr_state <- (curr_state %% 4) + 1      
      time_to_jump <- rexp(1, rate = 1)         
    }
    state_matrix[i, j] <- curr_state
  }
}
f_t <- colMeans(state_matrix == 1)

results_1000 <- data.frame(t_seq,f_t)

x_1000 <- 0:150
y_1000 <- theory_func(x_1000)

plot(x=results_1000$t_seq,y=results_1000$f_t,main="1000 Ind. Markov Chains",
     ylab="y_1(t)",xlab="Time")
lines(x=x_1000, y=y_1000,col="red",lwd=2)

#Simulating 10000 Independent Markov Chains

N <- 10000
t_seq <- seq(0, 5, length.out = 150)

state_matrix <- matrix(NA, nrow = N, ncol = length(t_seq))

set.seed(44)

for (i in 1:N) {
  
  if (runif(1) < 1/3) {
    curr_state <- 1
  } else {
    curr_state <- 2
  }
  
  curr_time <- 0
  
  time_to_jump <- rexp(1, rate = 1)
  
  for (j in 1:length(t_seq)) {
    target_time <- t_seq[j]
    
    
    while (curr_time + time_to_jump <= target_time) {
      curr_time <- curr_time + time_to_jump    
      curr_state <- (curr_state %% 4) + 1      
      time_to_jump <- rexp(1, rate = 1)         
    }
    state_matrix[i, j] <- curr_state
  }
}
f_t <- colMeans(state_matrix == 1)

results_10000 <- data.frame(t_seq,f_t)

plot(x=results_10000$t_seq,y=results_10000$f_t,main="10000 Ind. Markov Chains",
     ylab="y_1(t)",xlab="Time")
lines(x=x_1000, y=y_1000,col="red",lwd=2)


#Simulating 100000 Individual Markov Chains

N <- 100000
t_seq <- seq(0, 5, length.out = 150)

state_matrix <- matrix(NA, nrow = N, ncol = length(t_seq))

set.seed(45)

for (i in 1:N) {
  
  if (runif(1) < 1/3) {
    curr_state <- 1
  } else {
    curr_state <- 2
  }
  
  curr_time <- 0
  
  time_to_jump <- rexp(1, rate = 1)
  
  for (j in 1:length(t_seq)) {
    target_time <- t_seq[j]
    
    
    while (curr_time + time_to_jump <= target_time) {
      curr_time <- curr_time + time_to_jump    
      curr_state <- (curr_state %% 4) + 1      
      time_to_jump <- rexp(1, rate = 1)         
    }
    state_matrix[i, j] <- curr_state
  }
}


f_t <- colMeans(state_matrix == 1)

results_100000 <- data.frame(t_seq,f_t)

plot(x=results_100000$t_seq,y=results_10000$f_t,main="100000 Ind. Markov Chains",
     ylab="y_1(t)",xlab="Time")
lines(x=x_1000, y=y_1000,col="red",lwd=2)
