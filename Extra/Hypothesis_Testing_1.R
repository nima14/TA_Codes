data(women)

women$height_cm= women$height * 2.54
n <- 15
mu_0 <- 168
#mu_0 <- 175

mean_sample <- mean(women$height_cm)
S <- sd(women$height_cm)
S_sample <- S/sqrt(n)

shapiro.test(women$height_cm)

#two.sided

t.test(women$height_cm,mu=mu_0,alternative = 'two.sided',conf.level = 0.95)


t0 <- (mean_sample - mu_0)/ S_sample
acc_range <- c(- qt(0.975,n-1), qt(0.975,n-1))

acc_range_for_Xbar <- c(mu_0 - qt(0.975,n-1)*S_sample,mu_0 + qt(0.975,n-1)*S_sample)


p_value <- 2*pt(-abs(t0),n-1)
#conf_int <- c(mean_sample - qt(0.975,n-1)*S_sample,mean_sample + qt(0.975,n-1)*S_sample)
#----------------------------------------------
#one.sided

t.test(women$height_cm,mu=mu_0,alternative = 'less',conf.level = 0.95)

t0 <- (mean_sample - mu_0)/ S_sample
acc_range <- c(- qt(0.95,n-1), Inf)


acc_range_for_Xbar <- c(mu_0 - qt(0.95,n-1)*S_sample,Inf)



p_value <- pt(t0,n-1)
#conf_int <- c(-Inf,mean_sample + qt(0.95,n-1)*S_sample)

#---------------------------------------------

#Z Test

sd_pop <- 11
sd_sample <- sd_pop/sqrt(n)


z0 <- (mean_sample - mu_0)/ sd_sample
acc_range <- c(- qnorm(0.975), qnorm(0.975))


acc_range_for_Xbar <- c(mu_0 - qnorm(0.975)*S_sample,mu_0 + qnorm(0.975)*S_sample)


p_value <- 2*pnorm(-abs(z0))

#conf_int <- c(mean_sample - qnorm(0.975)*S_sample,mean_sample + qnorm(0.975)*S_sample)

#---------------------------------

#Chi2 Test for variance

#One.sided

var_0 <- 121
#var_0 <- 70
S2 <- var(women$height_cm)

library(EnvStats)

varTest(women$height_cm,sigma.squared =var_0,alternative = 'greater',conf.level = 0.95)


ch_0 <- (n-1)*S2/var_0
acc_range <- c(0,qchisq(0.95,n-1))


acc_range_for_S2 <- c(0,qchisq(0.95,n-1)*var_0/(n-1))


p_value <- 1- pchisq(ch_0,n-1) 

#conf_int <- c(S2*(n-1)/qchisq(0.95,n-1),Inf)
#---------------------------------------------

#two.sided
#var_0 <- 121
var_0 <- 60
S2 <- var(women$height_cm)

varTest(women$height_cm,sigma.squared =var_0,alternative = 'two.sided',conf.level = 0.95)




ch_0 <- (n-1)*S2/var_0
acc_range <- c(qchisq(0.025,n-1),qchisq(0.975,n-1))


acc_range_for_S2 <- c(qchisq(0.025,n-1)*var_0/(n-1),qchisq(0.975,n-1)*var_0/(n-1))



p_value <- 2*(1- pchisq(ch_0,n-1) ) 
#conf_int <- c(S2*(n-1)/qchisq(0.975,n-1),S2*(n-1)/qchisq(0.025,n-1))