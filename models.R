setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM Dataset 2/')

library(rpart)
library(tree)
library(party)
library(klaR)
library(caret)




raw.orig <- read.csv('Data/result.csv')

# x <- subset(raw.orig, select=-c(total,Unnamed..0.1))
# y <- raw.orig$total
# y<-discretize( y, method='frequency',categories = 5 )


model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10))

frmla <- raw.orig$total ~ raw.orig$idle_time + raw.orig$mouse_movement + raw.orig$keystroke + 
  raw.orig$time_delta + raw.orig$left_click + raw.orig$right_click 
 
fit <- rpart(frmla, method="class", data=raw.orig)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform=TRUE, main="Classification Tree for Chemicals")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

tr = tree(frmla, data=raw.orig)
summary(tr)
plot(tr); text(tr)


