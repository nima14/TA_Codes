#Manual
set.seed(45)
n1=15
Ch2_1 <- rchisq(100000,n1)

mean(Ch2_1)
var(Ch2_1)

n2=10
Ch2_2 <- rchisq(100000,n2)

mean(Ch2_2)
var(Ch2_2)

F1 <- (Ch2_1/n1) / (Ch2_2/n2)


mean(F1)
print(n2/(n2-2))

var(F1)
print(2*(n2^2)*(n2+n1-2)/(n1*((n2-2)^2)*(n2-4)) )
#-----------------------------------
C2_R <- rf(100000,15,10)
mean(C2_R)
var(C2_R)
#-------------------------------------

#Plotting
library(ggplot2)
library(ggfortify)

sp <- autoplot(density(F1), fill = 'green')
sp + geom_vline(xintercept = mean(F1))
#------------------------------------

# Using tables in books
1- pf(1.53,15,10)
qf(0.75,15,10)

#-----------------------------------

library(vistributions)
vdist_launch_app()

