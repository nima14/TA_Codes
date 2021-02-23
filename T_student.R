#Manual
Z <- rnorm(100000)
Chi2 <- rchisq(100000,100)
t <- Z/sqrt(Chi2/100)

mean(t)
sd(t)
#-------------------------------
#R
t_R <- rt(100000,100)

mean(t_R)
sd(t_R)
#----------------------------------

#Plotting
library(ggplot2)
library(ggfortify)

sp <- autoplot(density(t), fill = 'green')
sp + geom_vline(xintercept = mean(t))
#----------------------------

p <- ggdistribution(dt, seq(-5, 5, 0.1), df = 2, colour = 'blue')
p <- ggdistribution(dt, seq(-5,5, 0.1), df = 4, colour = 'green', p = p)
ggdistribution(dt, seq(-5,5, 0.1), df = 10, colour = 'red', p = p)

#-------------------------------


var(rt(100000,4))
var(rt(100000,5))
var(rt(100000,100))

#----------------------------

# Using tables in books
1- pt(2.571,5)
qt(1-0.025,5)