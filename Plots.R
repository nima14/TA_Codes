library(ggplot2)

#Histogram

set.seed(1234)
df <- data.frame(
  weight=rnorm(200, mean=65, sd=5)
)
head(df)


# Change the width of bins
ggplot(df, aes(x=weight)) + 
  geom_histogram(binwidth=5,color="black", fill="cyan")

#bins=10
#binwidth=20


#BoxPlot

ggplot(df, aes(x=1, y=weight)) + geom_boxplot(color="black", fill="cyan")

mean(df$weight)
quantile(df$weight)




set.seed(1234)
df1 <- data.frame(
  weight=c(rnorm(200, mean=65, sd=5),85)
)

mean(df1$weight)
quantile(df1$weight)

ggplot(df1, aes(x=1, y=weight)) +
  geom_boxplot(color="black", fill="cyan")


IQR <- quantile(df1$weight,0.75)-quantile(df1$weight,0.25)

c(quantile(df1$weight,0.25)-1.5*IQR,quantile(df1$weight,0.75)+1.5*IQR)



df1[df1$weight>77.83,]
df1[df1$weight<51.11016,]

