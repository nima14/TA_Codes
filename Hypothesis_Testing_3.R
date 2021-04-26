#Goodness od Fit Chi2
O <- c(22,24,15,21,22,16)

p <- rep(1/6,6)

chisq.test(O,p=p)


f <- sum(O)

E <- f*p

statistic_1 <- sum((O - E)^2/E)

df_1 <-length(O)-1


PValue_1 <- pchisq(statistic_1,df_1,lower.tail = FALSE)

acc_range_1 <- c(0,qchisq(0.05,df_1,lower.tail = FALSE))



#Independence Chi2

file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)


chisq <- chisq.test(housetasks)

col_sum <- colSums(housetasks)
col_n <- ncol(housetasks)

row_sum <- rowSums(housetasks)
row_n <- nrow(housetasks)

Total <- sum(col_sum)

E <- c()

for (i in col_sum){
    for (j in row_sum){
      
      E <- c(E,i*j/Total)
      
    }
}



Expected <- matrix(E,nrow=row_n,ncol=col_n)


statistic_2 <-sum((housetasks - Expected)^2/Expected)

df_2 <- (row_n-1)*(col_n-1)


PValue_2 <- pchisq(statistic_2,df_2,lower.tail = FALSE)

acc_range_2 <- c(0,qchisq(0.05,df_2,lower.tail = FALSE))

#ANOVA

df <-
data.frame(

Group = c(rep('A',6),rep('B',6),rep('C',6),rep('D',6)) ,
Obs = c(14,18,19,17,16,18,7,8,15,11,9,10,19,25,22,23,18,20,12,17,13,18,19,15)
)

one.way <- aov(Obs~Group,data=df)

summary(one.way)


pvalue <- pf(19.60521,3,20,lower.tail = FALSE)





