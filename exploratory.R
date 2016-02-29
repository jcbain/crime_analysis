# setwd(dir = 'Desktop/Spring 2016/CS 7001/Project_1/part_1/EPM Dataset 2/')

library(ggplot2)

sessions <- read.csv('Data/sessions.csv')
finalg <- read.csv('Data/final_grades.csv')
intg <- read.csv('Data/inter_grades.csv')

# data transformations
head(sessions[which(sessions$keystroke == 0),]) # I wanted to see if there was just no activity in those times in which there is no keystroke activity (or keystroke = 0)
                                              # however, it turns out that there is still quite a bit of activity
head(sessions[which(sessions$exercise == 'Study_Es'),])

# descriptive statistics
nrow(unique(finalg['student_id'])) # number of students who took the final exam

id_dups <- duplicated(finalg['student_id']) # find those who took the final exam twice
nrow(finalg[id_dups,]) # find the count of those who took the exam twice

summary(sessions$keystroke)


## a couple of plots ## 

# create a density plot of keystroke by exercise type 
ggplot(data = sessions, aes(x = keystroke)) + geom_density(aes(group = exercise, colour = exercise,  alpha = .03 )) +
  xlim(0,100) + ggtitle("Desity Plot of Keystroke by Exercise")

# create a scatter plot of the number of right clicks vs. number of left clicks and color by exercise
ggplot(data = sessions, aes(x = right_click, y = left_click)) + 
  geom_point(aes(colour = factor(exercise)),alpha = 9/10) +
  ggtitle("Right vs. Left Click by Exercise")

# create a histogram of mouse movement by exercise
ggplot(data = sessions, aes(x = mouse_movement)) + 
  geom_histogram(aes(colour = factor(exercise)),binwidth = 10) + xlim(0,1500) +
  ggtitle("Mouse Movement Histogram by Exersize")


##  create a function to run a min-max normalization on an column ##
normalize <- function(x){
  normalized = (x-min(x))/(max(x)-min(x))
  return(normalized)
}
