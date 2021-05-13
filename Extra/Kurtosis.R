library(moments)

x<- rnorm(1000)
print(kurtosis(x))

#----------------------------------------------

#Manual
mean_x=mean(x)
n=length(x)
m4=sum((x-mean_x)^4)/n
m2=sum((x-mean_x)^2)/n

skew_x=m4/m2^2

print(skew_x)

#---------------------------------------------

