#Main Distribution (Normal)
set.seed(1234)

n <-100000

df <- data.frame(
  x=rnorm(n, mean=180, sd=5)
)



ggplot(df, aes(x=x)) + 
  geom_histogram(bins=20,color="black", fill="cyan")


mean_df= mean(df$x)
var_df= var(df$x)

#Sampling Distribution

set.seed(25)

n_sample <- 200
sampling <- replicate(1000,sample(df$x,n_sample))

sampling_distribution <-  data.frame(
      x=colMeans(sampling)
      
)



ggplot(sampling_distribution, aes(x=x)) + 
  geom_histogram(bins=20,color="black", fill="cyan")


mean_sample= mean(sampling_distribution$x)
var_sample=  var(sampling_distribution$x)

var_df/n_sample



#----------------------------------------


#Main Distribution(Chi2)
set.seed(1234)

n <-100000

df <- data.frame(
  x=rchisq(n,5)
)



ggplot(df, aes(x=x)) + 
  geom_histogram(bins=20,color="black", fill="cyan")


mean_df= mean(df$x)
var_df= var(df$x)


#Sampling Distribution

set.seed(25)

n_sample <- 200
sampling <- replicate(1000,sample(df$x,n_sample))

sampling_distribution <-  data.frame(
  x=colMeans(sampling)
  
)



ggplot(sampling_distribution, aes(x=x)) + 
  geom_histogram(bins=20,color="black", fill="cyan")


mean_sample= mean(sampling_distribution$x)
var_sample=  var(sampling_distribution$x)

var_df/n_sample




#https://seeing-theory.brown.edu/probability-distributions/index.html#section3
