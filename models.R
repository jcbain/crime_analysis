setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM Dataset 2/')

library(rpart)
library(tree)
library(party)
library(klaR)
library(caret)

df<- read.csv('Data/output_data/merged.csv')
df<-na.omit(df)
df$session[df$session == 1] <- 'one'
df$session[df$session == 2] <- 'two'
df$session[df$session == 3] <- 'three'
df$session[df$session == 4] <- 'four'
df$session[df$session == 5] <- 'five'
df$session[df$session == 6] <- 'six'

x<- subset(df,select=-c(1:2,3,5))
y<- as.factor(df$session)

## train your model ##
model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10)) 
