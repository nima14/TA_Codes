p <- c(0.02,0.03,0.04)

compensate_price=300
n=5000
QA_price=7
repairing_price=15



E_A1 <- n*compensate_price*p
E_A2 <- n*(QA_price+repairing_price*p)


prior <- c(0.6,0.3,0.1)

E_A1_Total <- sum(E_A1 * prior)
E_A2_Total <- sum(E_A2 * prior)

