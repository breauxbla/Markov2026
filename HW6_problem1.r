install.packages("expm")
library(expm)

p <- matrix(c(0,1,0,0,0,
              1/3,0,2/3,0,0,
              0,1/2,0,1/2,0,
              0,0,2/3,0,1/3,
              0,0,0,1,0),nrow=5,byrow=TRUE)

p_50 <- p %^% 50

q <- matrix(c(0,0,1,0,0),nrow=1,byrow=TRUE)

q_50 <- q %*% p_50
q_50

pi_theory <- c(1/12,1/4,1/3,1/4,1/12)

plot_data2 <- rbind(pi_theory,q_50)

barplot(plot_data2,beside=TRUE,
        names.arg=0:4,col=c("darkgreen","red"),main="Theoretical S.D. (green) vs. q50 (red)",
xlab="States 0:4",ylab="Probability")
