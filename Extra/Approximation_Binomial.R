n=100
p=0.01
#p=0.6

q=1-p

a=2
#a=50



#Binomial
pbinom(a,n,p,lower.tail = FALSE)

#Normal
mean_norm =n*p
sd_norm = sqrt(n*p*q)

pnorm(a,mean_norm,sd_norm,lower.tail = FALSE)


#Poisson
lambda=n*p

ppois(a,n*p,lower.tail = FALSE)