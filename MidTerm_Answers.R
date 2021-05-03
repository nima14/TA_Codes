lambda <- c(1,2,3)

1 - (( (lambda^0)*exp(-lambda) ) + ( (lambda^1)*exp(-lambda) ))

p <-  1-ppois(1,lambda)


compensate_price=150
n=1000
QA_price=20
repairing_price=80
p_broken <- 0.1
broken_price <- 140

E_a1 <- n*compensate_price*p

E_a2 <- n*(QA_price+repairing_price*p+ (broken_price*p_broken*p))

#We choose E_a2 

prior <- c(0.85,0.1,0.05)

E_a1_Total <- sum(prior*E_a1)

E_a2_Total <- sum(prior*E_a2)

#We choose E_a1





alpha=0.05
beta=0.1
sd <- 5
mu0 <- 60
mu1 <- 58
n <- (qnorm(alpha,lower.tail = FALSE) + qnorm(beta,lower.tail = FALSE) )^2*(sd^2)/(mu1-mu0)^2

n <- ceiling(n)

X_bar <- 59.5

statistic <- (X_bar - mu0) / (sd/sqrt(n))

acc_range <- c(-qnorm(alpha,lower.tail = FALSE),Inf)

I_beta <- (sqrt(n)*(mu0-59)/sd) - qnorm(alpha,lower.tail = FALSE)

p <- pnorm(I_beta,lower.tail = FALSE)



