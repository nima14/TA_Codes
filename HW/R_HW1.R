set.seed(810994068)
Student_Number<-810994068
C=8
V=6
Vec_CV<-c(8,7)
X_Vec<-c(2,3)
Add_Vec<-X_Vec+Vec_CV
Mult_Vec<-X_Vec*Vec_CV
Mult_Sec<-Mult_Vec[2]

pnorm(5,4,V,lower.tail = FALSE)

qt(0.6,2*C)
qt(0.6,2*C,lower.tail = FALSE)

Rand_Norm<-rnorm(1000,C,V)

Rand_Mean<-mean(Rand_Norm)
Rand_sd<-sd(Rand_Norm)

Mat_Pois <- matrix(rpois(20,3*C),nrow=4,ncol=5)

Mat_SecRow<-Mat_Pois[2,]
Mat_ThirdCol<-Mat_Pois[,3]
Mat_Member<-Mat_Pois[4,2]
Mat_Great<-Mat_Pois[Mat_Pois>3*C]

beta_func<-function(u0,u1,sigma,n,alpha)
{
  p<-(u0-u1)/(sigma/sqrt(n)) -qnorm(alpha/2,lower.tail = FALSE)
  q<-(u0-u1)/(sigma/sqrt(n)) +qnorm(alpha/2,lower.tail = FALSE)
  return( pnorm(q)-pnorm(p) )
}

Beta<-beta_func(60,62,5,100,0.05)


#two.sided
n_func<-function(alpha,beta,sigma,u0,u1)
{
  K_alpha<-qnorm(alpha/2,lower.tail = FALSE)
  K_beta<-qnorm(beta,lower.tail = FALSE)
  
  n <- ceiling(((K_alpha+K_beta)*sigma/(u1-u0))^2)
  return(n)
}

n<-n_func(0.05,0.02,5,60,62)



X_Z<-qnorm(0.95)
Y_Z<-qnorm(0.025,lower.tail = FALSE)

sigma <- 12/(Y_Z - X_Z)
mu <- 156 - (X_Z*sigma)


