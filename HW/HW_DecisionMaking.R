p <- c( 1-pbinom(2,100,0.01) , 1-pbinom(2,100,0.015) , 1-pbinom(2,100,0.005) )


compensate_price=100
n=500
QA_price=5
repairing_price=30

E_a1 <- n*compensate_price*p



E_a2 <- n*(QA_price+repairing_price*p)

prior <- c(0.1,0.2,0.7)



E_a1 <- sum(prior*E_a1)

E_a2 <- sum(prior*E_a2)