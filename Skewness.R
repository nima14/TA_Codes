library(moments)
library(fGarch)
library(ggfortify)
library(EnvStats)
set.seed(45)
N <- 10000

#xi: 1 -> center , Greater than 1 -> Right Skewed , Lower than 1 -> Left Skewed
x=rsnorm(N, mean = 0, sd = 5, xi = 1)

hist(x, 
     probability=T, 
     col='lightblue')
lines(density(x,bw=1), col='red', lwd=3)

#----------------------------------------------
#R
print(skewness(x))
#-----------------------------------------------
#Manual
mean_x=mean(x)
n=length(x)
m3=sum((x-mean_x)^3)/n
m2=sum((x-mean_x)^2)/(n-1)

skew_x=m3/m2^(3/2)

print(skew_x)
#---------------------------------------------------

par(mfrow=c(1,2))

y <- rnorm(1000)
y_skewed <- c(rnorm(980),rep(15,20))


hist(y, 
     probability=T, 
     col='lightblue')
lines(density(y,bw=1), col='red', lwd=3)

hist(y_skewed, 
     probability=T, 
     col='lightblue')
lines(density(y_skewed,bw=1), col='red', lwd=3)


print(skewness(y))
print(skewness(y_skewed))

#----------------------------------------------------

z <- c(1,2,2,2,2,2,4)

z_skewed <- c(1,2,2,2,2,2,100)

print(skewness(z))
print(skewness(z_skewed))


#Manual
mean_z=mean(z)
mean_z_skewed=mean(z_skewed)
n=length(z)

m3_z=sum((z-mean_z)^3)/n
m3_z_skewed=sum((z_skewed-mean_z_skewed)^3)/n


m2_z=sum((z-mean_z)^2)/(n-1)
m2_z_skewed=sum((z_skewed-mean_z_skewed)^2)/(n-1)

skew_z=m3_z/m2_z^(3/2)
skew_z_skewed=m3_z_skewed/m2_z_skewed^(3/2)

#-------------------------------------------
#Removing Skewness
x=rsnorm(10000, mean = 100, sd = 5, xi = 5)
skewness(x)

x_log <- log(x)
skewness(x_log)


par(mfrow=c(2,1))
p=autoplot(density(x), fill = 'blue')
autoplot(density(x_log), fill = 'green')


