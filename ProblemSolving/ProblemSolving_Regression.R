
df <- data.frame(
  x= rep(c(0.5,1,1.5,2,2.5,3,3.5,4),3),
  
  y= c(56,123,176,200,270,345,401,432,61,101,160,215,243,370,426,461
       ,54,110,151,207,251,353,418,428)
)


fit <- lm(y~x,df)

summary(fit)

library(olsrr)

ols_pure_error_anova(fit)


#----------------------------------------

n <- 3
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





