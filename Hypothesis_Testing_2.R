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

t.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating,var.equal = TRUE)

Sp <- sqrt( ( (nx-1)*(Sx^2) + (ny-1)*(Sy^2) )/ (nx+ny-2) )
statistic_1 <- (X_bar - Y_bar)/(Sp*sqrt((1/nx)+(1/ny)))

df_1 <- nx+ny-2

alpha=0.05

acc_range_1 <- c(- qt(alpha/2,df_1,lower.tail =FALSE),qt(alpha/2,df_1,lower.tail =FALSE))

pvalue_1 <- pt(abs(statistic_1),df_1,lower.tail = FALSE)*2



#TTest Var Not Equal

t.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating,var.equal = FALSE)

statistic_2 <- (X_bar - Y_bar)/(sqrt(((Sx^2)/nx)+((Sy^2)/ny)))

df_2 <- ( ((((Sx^2)/nx) + ((Sy^2)/ny))^2)/ ( (((Sx^2)/nx)^2)/(nx-1) + (((Sy^2)/ny)^2)/(ny-1) ) ) 

acc_range_2 <- c(- qt(alpha/2,df_2,lower.tail =FALSE),qt(alpha/2,df_2,lower.tail =FALSE))


pvalue_2 <- pt(abs(statistic_2),df_2,lower.tail = FALSE)*2

#TTest Paired Data

before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)


n <- length(before)

D <- after - before

D_bar <- mean(D)
Sd <- sd(D)


t.test(after,before,paired = TRUE)

statistic_3 <- D_bar/(Sd/sqrt(n))

df_3 <- n-1

acc_range_3 <- c(- qt(alpha/2,df_3,lower.tail =FALSE),qt(alpha/2,df_3,lower.tail =FALSE))


pvalue_3 <-  pt(abs(statistic_3),df_3,lower.tail = FALSE)*2



#Z Test for Proportion 

nx <- 50
ny<- 70

Ax <- 22
Ay <- 18

Px <- Ax/nx
Py <- Ay/ny

Po <- (Ax+Ay)/(nx+ny)

statistic_4 <- (Px-Py) / sqrt(((Po*(1-Po))/nx) + ((Po*(1-Po))/ny))

acc_range_4 <- c(-qnorm(alpha/2,lower.tail = FALSE) , qnorm(alpha/2,lower.tail = FALSE) )





# F Test for Variance

var.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating)

statistic_5<- (Sx^2)/(Sy^2)

num_df <- nx-1
denom_df <- ny-1

acc_range_5 <- c(qf(1-(alpha/2),num_df,denom_df,lower.tail = FALSE),qf(alpha/2,num_df,denom_df,lower.tail = FALSE))
c(1/qf(alpha/2,denom_df,num_df,lower.tail = FALSE),qf(alpha/2,num_df,denom_df,lower.tail = FALSE))


pvalue_5 <- pf(statistic_4,num_df,denom_df)*2



#Reverse

var.test(Old_Films$IMDB_Rating,New_Films$IMDB_Rating)

statistic_6<- (Sy^2)/(Sx^2)

num_df <- ny-1
denom_df <- nx-1


acc_range_6 <- c(qf(1-(alpha/2),num_df,denom_df,lower.tail = FALSE),qf(alpha/2,num_df,denom_df,lower.tail = FALSE))

pvalue_6 <- pf(statistic_5,num_df,denom_df,lower.tail = FALSE)*2

