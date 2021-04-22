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
statistics_1 <- (X_bar - Y_bar)/(Sp*sqrt((1/nx)+(1/ny)))

df_1 <- nx+ny-2

pvalue_1 <- pt(abs(statistics_1),df_1,lower.tail = FALSE)*2



#TTest Var Not Equal

t.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating,var.equal = FALSE)

statistics_2 <- (X_bar - Y_bar)/(sqrt(((Sx^2)/nx)+((Sy^2)/ny)))

df_2 <- ( ((((Sx^2)/nx) + ((Sy^2)/ny))^2)/ ( (((Sx^2)/nx)^2)/(nx-1) + (((Sy^2)/ny)^2)/(ny-1) ) ) 


pvalue_2 <- pt(abs(statistics_2),df_2,lower.tail = FALSE)*2

#TTest Paired Data

before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)


n <- length(before)

D <- after - before

D_bar <- mean(D)
Sd <- sd(D)


t.test(after,before,paired = TRUE)

statistics_3 <- D_bar/(Sd/sqrt(n))

df_3 <- n-1

pvalue_3 <-  pt(abs(statistics_3),df_3,lower.tail = FALSE)*2



# F Test for Variance

var.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating)

statistic_4<- (Sx^2)/(Sy^2)

num_df <- nx-1
denom_df <- ny-1

pvalue_4 <- pf(statistic_4,num_df,denom_df)*2



#Reverse

var.test(Old_Films$IMDB_Rating,New_Films$IMDB_Rating)

statistic_5<- (Sy^2)/(Sx^2)

num_df <- ny-1
denom_df <- nx-1

pvalue_5 <- pf(statistic_5,num_df,denom_df,lower.tail = FALSE)*2

