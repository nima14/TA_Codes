# Dataframe

df_a <- data.frame(name=c('Nima','Ali','Amir'),Age=c(24,30,16))

df_IMDB <- read.csv('D:/TA/imdb_top_1000.csv')

Series_Title <- df_IMDB$Series_Title


q_79 <- df_IMDB[79,]

q_both <- df_IMDB[c(79,80),c("Series_Title","Genre")]

df_IMDB$Series_Title=='Vertigo'

q_vertigo <- df_IMDB[df_IMDB$Series_Title=='Vertigo',]


df_IMDB[df_IMDB$Released_Year==2017 & df_IMDB$IMDB_Rating>7.5 ,"Series_Title"]

# Not equal: !=    
# OR |


# install.packages('dplyr')

library(dplyr)

q_year_count <- df_IMDB %>% group_by(Released_Year) %>% summarise(n())

q_mean_Director <- df_IMDB %>% group_by(Director) %>%
          summarise(MeanRate=mean(IMDB_Rating)) %>% 
          arrange(-MeanRate)


df_IMDB[df_IMDB$Director=='Frank Darabont',"Series_Title"]




# Tests

data(women)


women$height_cm= women$height * 2.54

t.test(women$height_cm,mu=168,alternative = 'two.sided',conf.level = 0.95)



library(EnvStats)

varTest(women$height_cm,sigma.squared =121,alternative = 'two.sided',conf.level = 0.90)




df <- read.csv('D:/TA/imdb_top_1000.csv')


New_Films <- df[df$Released_Year>=1995,]
#New_Films <- df %>% filter(Released_Year>=1995)

Old_Films <- df[df$Released_Year<1995,]

t.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating,conf.level = 0.95,alternative = 'two.sided',var.equal = TRUE)
t.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating,conf.level = 0.95,alternative = 'two.sided',var.equal = FALSE)

var.test(New_Films$IMDB_Rating,Old_Films$IMDB_Rating,conf.level = 0.90,alternative = 'two.sided')



before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)


t.test(after,before,conf.level = 0.95,alternative = 'two.sided',paired = TRUE)






