library(ggplot2)
library(dplyr)
library(fields)
library(gridExtra)
library(grid)
library(Hmisc)
data <- read.table("ants/OutdoorDataset/Seq0006Object21Image64/gt/gt.txt", sep=",")
print(data)

ant_data <- function(ant_id){
  ant <- data[data$V2 == ant_id,]
  ant_list <- list(
    frame_no = c(),
    ant_id = c(),
    x = c(),
    y = c(),
    next_x = c(),
    next_y = c(),
    angle = c(),
    speed = c()
  )
  
  for (i in 1:nrow(ant)){
    for (j in 1:ncol(ant)){
      if(j == 1){
        ant_list$frame_no <- c(ant_list$frame_no, ant[i,j])
      }
      else if(j == 2){
        ant_list$ant_id <- c(ant_list$ant_id, ant[i,j])
      }
      else if(j == 3){
        l1 <- ant[i,j]
        
        if (i != nrow(ant)){
          l2 <- ant[i+1, j]
        }
      }
      else if(j == 4){
        t1 <- ant[i,j]
        if (i != nrow(ant)){
          t2 <- ant[i+1, j]
        }
      }
      else if(j == 5){
        w1 <- ant[i,j]
        if (i != nrow(ant)){
          w2 <- ant[i+1, j]
        }
      }
      else if(j == 6){
        h1 <- ant[i,j]
        if (i != nrow(ant)){
          h2 <- ant[i+1, j]
        }
      }
      else{
        x <- (l1 - w1)/2
        y <- (t1 - h1)/2
        
        ant_list$x <- c(ant_list$x, x)
        ant_list$y <- c(ant_list$y, y)
        
        if (i != nrow(ant)){
          x2 <- (l2 - w2)/2
          y2 <- (t2 - h2)/2
          ant_list$next_x <- c(ant_list$next_x, x2)
          ant_list$next_y <-c(ant_list$next_y, y2)
        }
      }
    }
  }
  
  for (x in 1:length(ant_list$x)-1){
    d_y <- ant_list$next_y[x] - ant_list$y[x]
    d_x <- ant_list$next_x[x] - ant_list$x[x]
    
    angle_rad <- atan2(d_y,d_x)
    
    ant_list$angle <- c(ant_list$angle, angle_rad)
    
    s <- ((ant_list$next_y[x] - ant_list$y[x])**2 + (ant_list$next_x[x] - ant_list$x[x])**2)**0.5
    ant_list$speed <- c(ant_list$speed, s)
  }
  
  len <- length(ant_list$angle)
  ant_list$next_x[len+1] <- 0 
  ant_list$next_y[len+1] <- 0
  ant_list$angle[len+1] <- 0
  ant_list$speed[len+1] <- 0
  
  df <- data.frame(frame_no = ant_list$frame_no,
                   ant_id = ant_list$ant_id,
                   x = ant_list$x,
                   y = ant_list$y,
                   next_x = ant_list$next_x,
                   next_y = ant_list$next_y,
                   angle = ant_list$angle,
                   speed = ant_list$speed
  )
  
  df <- df[-c(len+1), ]
  return(df)
}

a1 <- ant_data(11)

plot(a1$speed, type='l', xlab="Frames", ylab="Speed")

a1$speed_lag <- lag(a1$speed,1)
a1$speed_lead <- lead(a1$speed,1)
a1$change <- (a1$speed_lead - a1$speed_lag)

plot(a1$change, type='l', xlab="Frames", ylab="Speed")

ggplot(data=a1, aes(x=frame_no, y=speed))+
  geom_segment(aes(xend=frame_no,yend=speed+change), arrow=arrow(length=unit(.2,"cm")))+
  stat_density2d(aes(color=..level..))+
  labs(list(title="Vector Density Plot", x="Frames", y="Speed"))
