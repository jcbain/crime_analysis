setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM Dataset 2/')

library(rpart)
library(tree)
library(party)
library(klaR)
library(caret)

df <- read.csv('Data/result.csv')
df <- subset( df, select = -Unnamed..0 )

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
