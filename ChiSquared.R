#Manual
set.seed(45)
N1 <- rnorm(100000,5,4)

mean(N1)
sd(N1)

Z1 <- (N1-mean(N1))/sd(N1)

mean(Z1)
sd(Z1)

Z2 <- rnorm(100000)

mean(Z2)
sd(Z2)

Z3 <- rnorm(100000)

Z4 <- rnorm(100000)

C2 <- (Z1^2)+(Z2^2)+(Z3^2)+(Z4^2)

mean(C2)
var(C2)
#-----------------------------------
C2_R <- rchisq(100000,4)
mean(C2_R)
var(C2_R)
#-------------------------------------

#Plotting
library(ggplot2)
library(ggfortify)

sp <- autoplot(density(C2), fill = 'green')
sp + geom_vline(xintercept = mean(C2))
#------------------------------------

p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 2, colour = 'blue')
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 4, colour = 'green', p = p)
ggdistribution(dchisq, seq(0, 20, 0.1), df = 10, colour = 'red', p = p)

#------------------------------------
# Using tables in books
1- pchisq(0.71,4)

