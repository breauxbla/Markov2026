#q=1/2, p=s=14
#i=10

set.seed(123)

func_gamble <- function(i,p,s){
  q <- 1-p
  X <- i
  
  while(X>0){
    
    if(runif(1)<s){
      break
    } 
    if(runif(1)<p){
      X<-X+1
    }
    else{
      X<-X-1
    }
  }
  return(X)
}

i<-10
p<-1/4
q<-1/4
s<-1/2
N<-10000
N_2 <- 100000
N_3 <- 1000000

results <- replicate(N, func_gamble(i,p,s))
results_2 <- replicate(N_2, func_gamble(i,p,s))
results_3 <- replicate(N_3, func_gamble(i,p,s))

mean(results)
mean(results_2)
mean(results_3)
