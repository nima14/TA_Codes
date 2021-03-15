set.seed(78)

normal <- rnorm(1000,60,2)


mean(normal)
sd(normal)


#Plotting

library(ggplot2)
library(ggfortify)

sp <- autoplot(density(normal), fill = 'green')
sp + geom_vline(xintercept = mean(normal))

#----------------------------

p <- ggdistribution(dnorm, seq(-5, 5, 0.1),mean = 0, sd = 1, colour = 'blue')
p <- ggdistribution(dnorm, seq(-5,5, 0.1),mean = 0, sd = 2, colour = 'green', p = p)
ggdistribution(dnorm, seq(-5,5, 0.1),mean = 0, sd = 3, colour = 'red', p = p)


#----------------------------

#Standardization

normal <- rnorm(1000,60,2)
mean_normal <- mean(normal)
sd_normal <- sd(normal)

normal_standard <- (normal-mean_normal)/ sd_normal

mean(normal_standard)
sd(normal_standard)

normal

normal_standard


#---------------------------
# Using tables in books
1- pnorm(2.03)
1- pnorm(2.09)


qnorm(1-0.0749)
qnorm(1-0.0681)


pnorm(-2.03)
qnorm(0.0749)

