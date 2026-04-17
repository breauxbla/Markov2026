#HW8, Problem 2 solving Differential Equations Numerically

install.packages("deSolve")
library(deSolve)

model <- function(t, y, parms) {
  pF <- y[1]
  pN <- y[2]
  pD <- y[3]
  
  dpF <- pN + pD - 2*pF
  dpN <- pF - pN
  dpD <- pF - pD
  
  list(c(dpF, dpN, dpD))
}

y0 <- c(pF = 0, pN = 1, pD = 0)

times <- seq(0, 10, by = 0.01)

out <- ode(y = y0, times = times, func = model, parms = NULL)

pF_t <- out[, "pF"]

pF_t

plot(out[, "time"], pF_t, type = "l",
     xlab = "t", ylab = "P(F at time t)",
     main = "Probability Receptor is Free")
