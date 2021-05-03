#Variables

x <- 1
x
print(x)
y <- 2
x+y

#Vectors

vector_x <- c(5,10)

list(5,10)
vector_y <- c(7,10)

vector_x + vector_y
vector_x*2
vector_x^2

vector_x[1]
vector_x[2]

#Statsitical Functions in R

pnorm(-1)

pnorm(-1,lower.tail = FALSE)
1-pnorm(-1)

qnorm(0.05)

qnorm(0.05 , lower.tail = FALSE)
qnorm(1-0.05)

pt(-1,5)

qt(0.05,5)

pnorm(5,5,2)

set.seed(45)

x <- rnorm(100,20,1)

mean(x)
sd(x)
var(x)

y <- c(5,10,14,20,26)
median(y)


#Matrix

mat <- matrix(1:9,nrow=3,ncol =3)
mat

mat[1,2]
mat[1:2,2]
mat[,2]

mat>6
mat[mat>6]


vector_x>9
vector_x[vector_x>9]



#Functions

inch_to_centimeter <- function(x)
  {
  
  return(x*2.54)
}




inch_to_centimeter(5)




beta.z.one.sided <- function(mu0,mu1,sigma,n,alpha)
{

  q <- (mu0-mu1)/(sigma/sqrt(n)) - qnorm(alpha,lower.tail = FALSE)
  return(pnorm(q,lower.tail = FALSE))
}

beta.z.one.sided(mu0=60,mu1=59,sigma=3,n=20,alpha=0.05)


