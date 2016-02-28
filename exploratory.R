library(ggplot2)

session <- read.csv('Data/sessions.csv')
finalg <- read.csv('Data/final_grades.csv')
intg <- read.csv('Data/inter_grades.csv')

# create a function to run a min-max normalization on an column
normalize <- function(x){
  normalized = (x-min(x))/(max(x)-min(x))
  return(normalized)
}

# some new normalized variables
session['mouse_move_norm'] <- normalize(session['mouse_movement'])
session['keystroke_norm'] <- normalize(session['keystroke'])

mean_idle <- aggregate(idle_time ~ student_id, data=session, FUN=mean)
mean_final <- aggregate(total ~ student_id, data = finalg, FUN=mean)
idle_final <- merge(mean_idle,mean_final,by="student_id")


# some stats
summary(session)
summary(finalg)
summary(intg)

summary(session['mouse_movement'])
summary(session['keystroke'])
cor(session['mouse_movement'],session['keystroke'])

# a couple of plots 
qplot(total, data = finalg, geom= "histogram")
ggplot(data = finalg, aes(x=total)) + geom_histogram(binwidth = 5)
ggplot(data = session, aes(x = mouse_move_norm, y = keystroke_norm)) + 
  geom_bin2d(bins = 50) + geom_density2d()
ggplot(data = session, aes(x = keystroke_norm)) + geom_histogram()
