library(ggplot2)

# creat a function to run a min-max normalization on an column
normalize <- function(x){
  normalized = (x-min(x))/(max(x)-min(x))
  return(normalized)
}

session <- read.csv('Data/sessions.csv')

qplot(mouse_movement, data = session, geom= "histogram")
ggplot(data = session, aes(x = mouse_movement)) + geom_density()
