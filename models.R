setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM Dataset 2/')

library(rpart)
library(tree)

raw.orig <- read.csv('Data/result.csv')

raw <- subset(raw.orig, select = c('total', 'idle_time','mouse_movement','keystroke','time_delta'))

frmla <- raw.orig$total ~ raw.orig$idle_time + raw.orig$mouse_movement + raw.orig$keystroke + raw.orig$time_delta +
  raw.orig$es_6_1_25points + raw.orig$es_4_2_10points
fit <- rpart(frmla, method="class", data=raw.orig)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform=TRUE, main="Classification Tree for Chemicals")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

tr = tree(frmla, data=raw.orig)
summary(tr)
plot(tr); text(tr)
