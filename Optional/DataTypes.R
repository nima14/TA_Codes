library(ggplot2)

df <- read.csv('D:/Titanic.csv')

df <- df[df$Embarked %in% c('S','Q','C'),]

#Numerical One Variable

#Histogram

ggplot(df, aes(x=Fare)) + 
  geom_histogram(color="black", fill="cyan")

#BoxPlot
ggplot(df, aes(x=1, y=Fare)) + 
  geom_boxplot(color="black", fill="cyan")



#Categorical One Variable

#Freq Table
table(df$Embarked)


#Bar Chart
ggplot(data=df, aes(x=Sex)) +
  geom_bar(color="black", fill="cyan")


#Pie Chart
ggplot(data=df, aes(x=factor(1), fill=Embarked)) +
  geom_bar(width = 1) +
  coord_polar("y")



#Categorical Multiple Variables

#Bar Chart
ggplot(data=df, aes(x=Embarked,fill=c(Sex))) +
  geom_bar(position="dodge")


#Categorical & Numerical Data Ensemble

#Bar Chart
ggplot(data=df, aes(x=Sex,y=Fare,fill=Sex)) +
  geom_col()

#BoxPlot

ggplot(df, aes(x=Sex, y=Age,fill=Sex)) + 
  geom_boxplot()


#Numerical Multiple Variables

#Scatter Plot
ggplot(data=df, aes(x=Fare,y=Age,color='red')) +
  geom_point()



# 2 Numerical  & 1 Categorical Variables
ggplot(data=df, aes(x=Fare,y=Age,color=Sex)) +
  geom_point()






