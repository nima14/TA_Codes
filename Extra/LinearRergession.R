#Simple Linear Regression
library(UsingR)
library(ggplot2)

data("Galton")

fit <- lm(data=Galton,child~parent)

summary(fit)$coef

ggplot(Galton, aes(x=parent, y=child)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')



# Corrrelation
set.seed(5)
x <- rnorm(100)
y <- 2*x+runif(100,-1,1)

cor(x,y)

df <- data.frame(x=x,y=y)

ggplot(df, aes(x=x, y=y)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')

z <- -2*x+runif(100,-1,1)

cor(x,z)

df <- data.frame(x=x,z=z)

ggplot(df, aes(x,z)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')

u <- rnorm(100)

cor(x,u)

df <- data.frame(x=x,u=u)

ggplot(df, aes(x,u)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')


# LR Calculation




S_xy <- sum((Galton$parent-mean(Galton$parent))*(Galton$child-mean(Galton$child)))
S_xx <- sum((Galton$parent-mean(Galton$parent))^2)

b <- S_xy/S_xx
a <- mean(Galton$child) - b*mean(Galton$parent)

#---------------------------------

SSE <-  sum(resid(fit)^2)

n <- length(Galton$parent)
Sigma_S_xy <- sqrt(SSE/(n-2))

SE_A <- sqrt( ((1/n)+(mean(Galton$parent)^2/S_xx) ) ) * Sigma_S_xy
SE_B<- Sigma_S_xy / ( sqrt(S_xx) )


stat_A <- a/SE_A
stat_B <- b/SE_B

PValue_A <- 2*pt(abs(stat_A) , df= n-2 , lower.tail=FALSE )
PValue_B <- 2*pt(abs(stat_B) , df= n-2 , lower.tail=FALSE )

Galton <- cbind(Galton,Pred=predict(fit,newdata = Galton))

#----------------------------------

parent <- 68

#Confidence Interval

predict(fit,newdata = data.frame(parent),interval ="confidence" )

(a+b*parent) +c(-1,1)*qt(0.025,df=n-2,lower.tail = FALSE)*Sigma_S_xy*sqrt((1/n)+( parent - mean(Galton$parent))^2/S_xx)

#Predict

predict(fit,newdata = data.frame(parent),interval ="prediction" )

(a+b*parent) +c(-1,1)*qt(0.025,df=n-2,lower.tail = FALSE)*Sigma_S_xy*sqrt(1+(1/n)+( parent - mean(Galton$parent))^2/S_xx)


# Lack of Fit Test

df <- data.frame(
x= rep(c(4.2,4.6,5.2,6,6.7,7.3,8),2),

y= c(5.1,7,6.3,8.4,12.3,10.3,13.3,5.9,5.4,6.8,7.3,11.1,12.7,12.7)
            )


fit <- lm(y~x,df)

summary(fit)

library(olsrr)

ols_pure_error_anova(fit)


#----------------------------------------
 
n <- 2
k <- length(df$x)/n

df_LOF <- k-2
df_PE <- k*(n-1)


library(dplyr)

df_meanY <- df %>% group_by(x) %>% summarise(Mean_Y=mean(y))

df_meanY <-data.frame(x=rep(df_meanY$x,2),Mean_Y=rep(df_meanY$Mean_Y,2))

pred_Y <- predict(fit)

SS_LOF <- sum((df_meanY$Mean_Y - pred_Y)^2)

SS_PE <- sum((df$y - df_meanY$Mean_Y)^2)

MS_LOF <- SS_LOF/df_LOF
MS_PE <- SS_PE/df_PE

statistic <- MS_LOF/MS_PE

pvalue <- pf(statistic,df_LOF,df_PE,lower.tail = FALSE)
  


#------------------------------------

df <- data.frame(
Year <- c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016),
Month <- c(12, 11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1),
Interest_Rate <- c(2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,2,2,2,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75),
Unemployment_Rate <- c(5.3,5.3,5.3,5.3,5.4,5.6,5.5,5.5,5.5,5.6,5.7,5.9,6,5.9,5.8,6.1,6.2,6.1,6.1,6.1,5.9,6.2,6.2,6.1),
Stock_Index_Price <- c(1464,1394,1357,1293,1256,1254,1234,1195,1159,1167,1130,1075,1047,965,943,958,971,949,884,866,876,822,704,719)
                )

model <- lm(data=df,Stock_Index_Price ~ Interest_Rate + Unemployment_Rate)
summary(model)


ggplot(df, aes(x=Interest_Rate, y=Stock_Index_Price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')


ggplot(df, aes(x=Unemployment_Rate, y=Stock_Index_Price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')
