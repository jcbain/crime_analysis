setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM Dataset 2/')

library(rpart)
library(tree)
library(party)
library(klaR)
library(caret)

######################
## read in the file ##
######################
df<- read.csv('Data/output_data/merged.csv') # use the merged data/ be sure to change to the relevant path
df<-subset(df,select=-c(1,2,4,5)) # take out the dumb columns/ the columns that are not independent of the target
df<-na.omit(df) # omit na's from the file
df$session[df$session == 1] <- 'one' # ugly way of doing this/ also not necessary
df$session[df$session == 2] <- 'two'
df$session[df$session == 3] <- 'three'
df$session[df$session == 4] <- 'four'
df$session[df$session == 5] <- 'five'
df$session[df$session == 6] <- 'six'

df$session<-as.factor(df$session) # make sure that you factorize the target

#################
## NAIVE BAYES ##
#################
#***************#


######################################
## define training and testing sets ##
######################################
sub<-sample(nrow(df),floor(nrow(df)*0.9)) # training data is 90% of all data (random rows)
train<- df[sub,] # define training data from the sub
test<- df[-sub,] # define the testing data from the rest

xTrain<- subset(train,select=-c(session)) # inputs
yTrain<- as.factor(train$session) # targets

xTest<- subset(test,select=-c(session))
yTest<- as.factor(test$session)

######################
## train your model ##
######################
model <- train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

############################################
## see how model performs on testing data ##
############################################
prop.table(table(predict(model$finalModel,xTest)$class,yTest))

####################
## DECISION TREES ##
####################
# *****************#

## model ##
fit <- rpart(session ~ ., df)
summary(fit)


## plot decision tree ##
tr = tree(fit, data=df)
summary(tr)
plot(tr); text(tr)


### Predicting session based on hardware behavior was a rather crappy model ###
###############################################################################
#df<- read.csv(('Data/output_data/sessions.csv'))
#df<-na.omit(df)
#sub<-sample(nrow(df),floor(nrow(df)*0.01))
#train<- df[sub,]
#test<- df[-sub,]
#xTrain<-subset(train,select=-c(1:7))
#yTrain<-as.factor(train$session)





