setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM_Dataset/')

library(rpart)
library(tree)
library(party)
library(klaR)
library(caret)

df <- read.csv('Data/output_data/result.csv')
df <- subset( df, select = -c(Unnamed..0,Unnamed..0.1) )

x <- subset(df, select = -total)
y <- round(df$total, digits = -1)

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
