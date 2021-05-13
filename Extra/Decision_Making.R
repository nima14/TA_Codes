#Calculating Probability 

#Page 171

mean_1=72
mean_2=75

std=2


lower=68
upper=76


lower_z_1= (lower - mean_1)/std

upper_z_1= (upper - mean_1)/std



Prob_1= 1- (pnorm(upper_z_1) - pnorm(lower_z_1) )




lower_z_2= (lower - mean_2)/std

upper_z_2= (upper - mean_2)/std



Prob_2= 1- (pnorm(upper_z_2) - pnorm(lower_z_2) )

#----------------------------------------------------

#Page 172

#a1 (Buying 500 without sign)
n=500
Repairing_Cost=150
Buy_Price=70
E_1_A1 = n *(Buy_Price+ Repairing_Cost * Prob_1)
E_2_A1 =  n *(Buy_Price+ Repairing_Cost * Prob_2)


#a2 (Buying 500 From A)
n=500
Repairing_Cost=150
Buy_Price=100
E_A2 = n *(Buy_Price+ Repairing_Cost * Prob_1)


#a3 (Buying 250 without sign & 250 From A)
n=250
Repairing_Cost=150
Buy_Price_Without_Sign=75
Buy_Price_From_A=100

E_1_A3 = n *(Buy_Price_Without_Sign + Repairing_Cost * Prob_1) +
         n *(Buy_Price_From_A + Repairing_Cost * Prob_1)

E_2_A3 = n *(Buy_Price_Without_Sign + Repairing_Cost * Prob_2) +
         n *(Buy_Price_From_A + Repairing_Cost * Prob_1)

#----------------------------------------------

#E(l) based on Prior

#Page 169

Prior_1=2/3
Prior_2=1/3

(E_1_A1 * Prior_1) + (E_2_A1 * Prior_2)

(E_A2 * Prior_1) + (E_A2 * Prior_2)

(E_1_A3 * Prior_1) + (E_2_A3 * Prior_2)


#-----------------------------------------------

# https://www.mathsisfun.com/data/probability-events-conditional.html

# https://seeing-theory.brown.edu/bayesian-inference/index.html#section1

#Calculating Posterior
#Page 190

mean_sample=74.5
size_sample=4

std_sample=std/sqrt(size_sample)



Likelihood <- function(mean_sample,mean,std) 
  {
    (1/(sqrt(2*3.14)*std))*exp(-1*((mean_sample-mean)**2)/(2*(std)**2))
    
  }

Likelihood_1 =Likelihood(mean_sample,mean_1,std_sample) 

Likelihood_2 =Likelihood(mean_sample,mean_2,std_sample)


Posterior_1 = (Prior_1 * Likelihood_1)/ ( (Prior_1*Likelihood_1) + (Prior_2*Likelihood_2))

Posterior_2 = (Prior_2 * Likelihood_2)/ ( (Prior_1*Likelihood_1) + (Prior_2*Likelihood_2))

#--------------------------------------

#E(l) based on Prior

#Page 187



(E_1_A1 * Posterior_1) + (E_2_A1 * Posterior_2)

(E_A2 * Posterior_1) + (E_A2 * Posterior_2)

(E_1_A3 * Posterior_1) + (E_2_A3 * Posterior_2)
