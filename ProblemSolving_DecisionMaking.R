#Example Page 175




prior <- c(0.1,0.5,0.4)

E_A2 <- c(203,95,33)
E_A3 <- c(64,51,45)


sum(E_A2 * prior)

sum(E_A3 * prior)


Likelihood <- c(ppois(13,24) - ppois(12,24) , ppois(13,20) - ppois(12,20) , ppois(13,16) - ppois(12,16))

numerator <- Likelihood*prior

denomerator <- sum(Likelihood*prior)

posterior <- numerator/denomerator

sum(E_A2 * posterior)

sum(E_A3 * posterior)
#---------------------------------

#Question 4

additional_cost <- 70
repairing_cost <- 20
QA_Cost <- 2
n <- 10000

p <- c(0.01,0.05,0.1)

a1 <- n*additional_cost*p
a2 <- n*(QA_Cost+20*p)

prior <- c(0.2,0.6,0.2)

a1_Bayes_res_prior <- sum(a1*prior)
a2_Bayes_res_prior <- sum(a2*prior)

#---------------------------------------------
#Question 14

mean_1=72
mean_2=75

std=2

#mean_sample=73.5
mean_sample=74
size_sample=4

std_sample=std/sqrt(size_sample)

Prior_1=2/3
Prior_2=1/3



Likelihood <- function(mean_sample,mean,std) 
{
  (1/(sqrt(2*3.14)*std))*exp(-1*((mean_sample-mean)**2)/(2*(std)**2))
  
}


Likelihood_1 =Likelihood(mean_sample,mean_1,std_sample) 

Likelihood_2 =Likelihood(mean_sample,mean_2,std_sample)


Posterior_1 = (Prior_1 * Likelihood_1)/ ( (Prior_1*Likelihood_1) + (Prior_2*Likelihood_2))

Posterior_2 = (Prior_2 * Likelihood_2)/ ( (Prior_1*Likelihood_1) + (Prior_2*Likelihood_2))

#---------------------------------------------

# Question 26

mean <- 73.5
std <- 2
sample_size <- 4
sample_std <- std/sqrt(sample_size)

acc_range <- c(70.7,73.3)

acc_range_z <- (acc_range - mean)/sample_std

pnorm(acc_range_z)[2] - pnorm(acc_range_z)[1]