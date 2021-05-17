#Correlation

set.seed(5)
x <- rnorm(100)
y <- 2*x+runif(100,-1,1)

cor(x,y)


# Linear Regression
library(UsingR)
library(ggplot2)

data("Galton")

fit <- lm(data=Galton,child~parent)

summary(fit)

ggplot(Galton, aes(x=parent, y=child)) + 
  geom_point() + 
  geom_smooth(method=lm)


# Predict 

predict(fit,newdata = Galton)

new_d <- 68

predict(fit,newdata = data.frame(parent=new_d),interval ="confidence",level=0.95)

predict(fit,newdata = data.frame(parent=new_d),interval ="prediction",level=0.95 )


# Lack of Fit Test


library(olsrr)

ols_pure_error_anova(fit)

