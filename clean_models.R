setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM_Dataset/')

library(rpart)
library(tree)
library(party)
library(klaR)
library(caret)

mround <- function(x,base){ 
  base*round(x/base) 
} 

#####################
## NAIVE BAYES !!! ##
#####################

## read in csv file ##
#df <- read.csv('Data/output_data/result.csv')
#df <- subset( df, select = -c(Unnamed..0,Unnamed..0.1) ) # clean up residual index rows
df<- read.csv('Data/output_data/deeds.csv')

## define training set and target ##
#x <- subset(df, select = -c(1:9,total)) # define training set 
x <- subset(df,select = c(es_6_1_25points,es_5_2_10points,es_4_2_10points,es_6_2_15points,es_5_3_3points,mouse_movement))
#x<-subset(df,select = -c(1,11:26))
#y <- as.factor(round(df$total, digits = -1)) # define target, discretize 
y <-as.factor(mround(df$total,10))

## train your model ##
model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10)) 

predict(model$finalModel,x) 
predict(model$finalModel,x)$class

table(predict(model$finalModel,x)$class,y)

#################
# decision tree #
#################

## model ##
fit <- rpart(total ~ ., df)
summary(fit)


## plot decision tree ##
tr = tree(fit, data=df)
summary(tr)
plot(tr); text(tr)
