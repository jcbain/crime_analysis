setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM_Dataset/')

library(rpart)
library(tree)
library(party)
library(klaR)
library(caret)
library(reshape2)
library(GGally)

####################################### #*#################*# #######################################
####################################### #*#################*# #######################################
####################################### #*#### PREDICT ####*# #######################################
####################################### #*#### SESSION ####*# #######################################
####################################### #*#################*# #######################################
####################################### #*#################*# #######################################

######################
## read in the file ##
######################
df<- read.csv('Data/output_data/merged.csv') # use the merged data/ be sure to change to the relevant path
df<-subset(df,select=-c(1,2,4,5)) # take out the dumb columns/ the columns that are not independent of the target
#df<-subset(df,select=-c(1,2,4,5,9)) # subset out time_delta feature
df<-na.omit(df) # omit na's from the file
df<-df[-df$idle_time < 5000,] # remove an extreme outlier in idle time
#df$session[df$session == 1] <- 'one' # ugly way of doing this/ also not necessary
#df$session[df$session == 2] <- 'two'
#df$session[df$session == 3] <- 'three'
#df$session[df$session == 4] <- 'four'
#df$session[df$session == 5] <- 'five'
#df$session[df$session == 6] <- 'six'

df$session<-as.factor(df$session) # make sure that you factorize the target

#################
## NAIVE BAYES ##
#################
#***************#


######################################
## define training and testing sets ##
######################################
sub<-sample(nrow(df),floor(nrow(df)*0.5)) # training data is 50% of all data (random rows)
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

preds<- predict(model$finalModel,xTest)$class 
reals<- yTest

vals<-cbind(preds,reals)
vals<-data.frame(vals)

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

###########
## PLOTS ##
###########
#*********#

##############################################
## function to plot multiple plots together ##
##############################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## create the individual plots ##
p1<- ggplot(df, aes(factor(session),idle_time, colour = session)) 
p2<- ggplot(df, aes(factor(session),right_click, colour = session)) 
p3<- ggplot(df, aes(factor(session),left_click, colour = session)) 
p4<- ggplot(df, aes(factor(session),mouse_wheel, colour = session)) 
p5<- ggplot(df, aes(factor(session),mouse_movement, colour = session)) 
p6<- ggplot(df, aes(factor(session),keystroke, colour = session)) 
p7<- ggplot(df, aes(factor(session),time_delta, colour = session))  

## boxplots ##
p1<-p1 + geom_boxplot() + theme(legend.position="none") + 
  labs(title = "idle time", x = "session", y = "") + coord_cartesian(ylim= c(18300,10000000))
p2<-p2 + geom_boxplot() + theme(legend.position="none") + 
  labs(title = "right click", x = "session", y = "")
p3<-p3 + geom_boxplot() + theme(legend.position="none") + 
  labs(title = "left click", x = "session", y = "") + coord_cartesian(ylim= c(0,25))
p4<-p4 + geom_boxplot() + theme(legend.position="none") + 
  labs(title = "mouse wheel", x = "session", y = "") + coord_cartesian(ylim= c(0,30))
p5<-p5 + geom_boxplot() + theme(legend.position="none") + 
  labs(title = "mouse movement", x = "session", y = "") + coord_cartesian(ylim= c(0,1700))
p6<-p6 + geom_boxplot() + theme(legend.position="none") + 
  labs(title = "keystroke", x = "session", y = "") + coord_cartesian(ylim= c(0,40))
p7<-p7 + geom_boxplot() + theme(legend.position="none") + 
  labs(title = "time", x = "session", y = "") + coord_cartesian(ylim= c(0,90))

## string plots together ##
multiplot(p6,p3,p7 ,p2, p1,p4,p5, cols=7)

## plots with variables plotted agains time ##
p8<- ggplot(df, aes(keystroke, colour = session)) 
p9<- ggplot(df, aes(idle_time, colour = session))
p10<- ggplot(df, aes(mouse_wheel, colour = session))
p11<- ggplot(df, aes(left_click, colour = session))
p12<- ggplot(df, aes(right_click, colour = session))
p13<- ggplot(df, aes(mouse_movement, colour = session))

## plot 2D density plots ##
p8 +  geom_density() + facet_wrap(~session)
p9 +  geom_density() + facet_wrap(~session)
p10 +  geom_density() + facet_wrap(~session)
p11 +  geom_density() + facet_wrap(~session)
p12 +  geom_density() + facet_wrap(~session)
p13 +  geom_density() + facet_wrap(~session)


## scatters ##
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Na%C3%AFve_Bayes
pairs(df[2:5],pch=21,bg=c("red","green3","blue","yellow","purple")[unclass(df$session)])


####################################### #*#################*# #######################################
####################################### #*#################*# #######################################
####################################### #*#### PREDICT ####*# #######################################
####################################### #*#### GRADES  ####*# #######################################
####################################### #*#################*# #######################################
####################################### #*#################*# #######################################


#####################
## NAIVE BAYES !!! ##
#####################
#*******************#

mround <- function(x,base){ 
  base*round(x/base) 
} 

## read in csv file ##
df2 <- read.csv('Data/output_data/final_grades.csv')
df2$rounded<- as.factor(mround(df2$total,10))
df2 <- subset(df2, select = -X ) # clean up residual index rows
df2 <- na.omit(df2)

x <- subset(df2, select = c(es_6_1_25points,es_5_2_10points, es_4_1_15points,es_3_3_2points) ) # define training set 
y <- as.factor(mround(df2$total,10))

## train your model ##
model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10)) 

predict(model$finalModel,x) 
predict(model$finalModel,x)$class

table(predict(model$finalModel,x)$class,y)

## scatter plots ##
p21<- ggplot(df2, aes(x=es_6_1_25points,y=total)) + geom_point(aes(colour=rounded)) +
  labs(x = 'section 6.2', colour = 'rounded totals')
p22<- ggplot(df2, aes(x=es_5_2_10points,y=total)) + geom_point(aes(colour=rounded)) +
  labs(x = 'section 5.2', colour = 'rounded totals')
p23<- ggplot(df2, aes(x=es_4_1_15points,y=total)) + geom_point(aes(colour=rounded)) +
  labs(x = 'section 4.1', colour = 'rounded totals')
p24<- ggplot(df2, aes(x=es_3_3_2points,y=total)) + geom_point(aes(colour=rounded)) +
  labs(x = 'section 3.3', colour = 'rounded totals')

multiplot(p21,p22,p23,p24, cols=2)

#################
# decision tree #
#################

## model ##
df2 <- read.csv('Data/output_data/final_grades.csv')
fit <- rpart(total ~ ., df2)
summary(fit)


## plot decision tree ##
tr = tree(fit, data=df2)
summary(tr)
plot(tr); text(tr)

#######################
## LINEAR REGRESSION ##
#######################
linM<- lm(df2$total~df2$es_6_2_15points+df2$es_5_2_10points+df2$es_4_1_15points+df2$es_3_3_2points)
summary(linM)

## add fitted values to dataframe ##
df2$fitted<-linM$fitted.values

## define a function ##
fun1 <-  function(x){
  10.8847 + (x*2.4959) + (mean(df2$es_5_2_10points)*1.5899) +
    (mean(df2$es_4_1_15points)*1.7154) + (mean(df2$es_3_3_2points)*2.7065 )
}

## plot with function ##
ggplot(df2, aes(x=es_6_2_15points,y=fitted)) + geom_point() +stat_function(fun = fun1)


