#One Parameter

alpha=0.05

data(women)

women$height_cm= women$height * 2.54
n <- 15

mu_0 <- 168

mean_sample <- mean(women$height_cm)
S <- sd(women$height_cm)


#T Test

#two.sided

t.test(women$height_cm,mu=mu_0,alternative = 'two.sided',conf.level = 0.95)

c(mean_sample - qt(alpha/2,n-1,lower.tail = FALSE)*S/sqrt(n),mean_sample + qt(alpha/2,n-1,lower.tail = FALSE)*S/sqrt(n))
#----------------------------------------------
#one.sided

t.test(women$height_cm,mu=mu_0,alternative = 'less',conf.level = 0.95)

c(-Inf,mean_sample + qt(alpha,n-1,lower.tail = FALSE)*S/sqrt(n))
#---------------------------------

#Chi2 Test for variance

#One.sided


S2 <- var(women$height_cm)
var_0 <- 121

library(EnvStats)

varTest(women$height_cm,sigma.squared =var_0,alternative = 'greater',conf.level = 0.95)


c(S2*(n-1)/qchisq(alpha,n-1,lower.tail = FALSE),Inf)
#---------------------------------------------

#two.sided

S2 <- var(women$height_cm)

varTest(women$height_cm,sigma.squared =var_0,alternative = 'two.sided',conf.level = 0.95)

c(S2*(n-1)/qchisq(0.975,n-1),S2*(n-1)/qchisq(0.025,n-1))





#Two Parameter



library(dplyr)
df <- read.csv('D:/TA/imdb_top_1000.csv')

New_Films <- df %>% filter(Released_Year>=1995)


Old_Films <- df %>% filter(Released_Year<1995)


X_bar <- mean(New_Films$IMDB_Rating)
Y_bar <- mean(Old_Films$IMDB_Rating)

Sx <- sd(New_Films$IMDB_Rating)
Sy <- sd(Old_Films$IMDB_Rating)

nx <- length(New_Films$IMDB_Rating)
ny <- length(Old_Films$IMDB_Rating)



#TTest Var=Equal

Sp <- sqrt( ( (nx-1)*(Sx^2) + (ny-1)*(Sy^2) )/ (nx+ny-2) )
df_1 <- nx+ny-2

t.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating,var.equal = TRUE)

(X_bar - Y_bar) +c(-1,1)* qt(alpha/2,df_1,lower.tail = FALSE)*sqrt((1/nx)+(1/ny))*Sp



# F Test for Variance

nx <- length(New_Films$IMDB_Rating)
ny <- length(Old_Films$IMDB_Rating)

num_df <- nx-1
denom_df <- ny-1

var.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating)


c( (Sx^2/Sy^2)/qf(alpha/2,num_df,denom_df,lower.tail = FALSE),(Sx^2/Sy^2)/qf(1-(alpha/2),num_df,denom_df,lower.tail = FALSE))




