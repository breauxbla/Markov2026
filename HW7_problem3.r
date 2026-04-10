#Markov HW7, Problem 3c

rate_func <- function(t){
  1/2+1/2*(t/30)^2
}

#Sample arrival times using exp(120)
set.seed(144)
arrival_times <- c()
current_time <- 0

while(current_time<120){
  u<-runif(1,0,1)
  T=-1/120 * log(1-u)
  
 current_time <- current_time + T
 if(current_time<120){
   arrival_times <- c(arrival_times, current_time)
 }
 
}

#Either Keep or don't arrival times with probability rate_func(T_i)/120
kept_times <- c()
for (i in 1:length(arrival_times)){
  T_eye <- arrival_times[i]
  val <- rate_func(T_eye)
  p <- val/120
  accept <- rbinom(1,1,p)
  
  if(accept==1){
   kept_times <- c(kept_times,T_eye) 
  }
}

#Histogram for Reports Per Day
hist(kept_times, 
     breaks=seq(0,120,by=1),main="Simulated Reports Per Day",
     xlab="Day",ylab="Reports",col="lightblue",border="black")
curve(0.5+0.5*(x/30)^2,col="red",lwd=2,add=TRUE)

#Simulated number of reports
length(kept_times)
