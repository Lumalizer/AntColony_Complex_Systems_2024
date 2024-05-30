library(ggplot2)
library(dplyr)
library(fields)
library(gridExtra)
library(grid)
library(Hmisc)
data <- read.table("ants/IndoorDataset/Seq0001Object10Image94/gt/gt.txt", sep=",")
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



ant1 <- ant_data(11)
ant2 <- ant_data(12)
ant3 <- ant_data(13)

x1 <- ant1$x[1:length(ant1$angle)]
x2 <- ant2$x[1:length(ant2$angle)]
x3 <- ant3$x[1:length(ant3$angle)]

y1 <- ant1$y[1:length(ant1$angle)]
y2 <- ant2$y[1:length(ant2$angle)]
y3 <- ant3$y[1:length(ant3$angle)]

u1 <- cos(ant1$angle)
u2 <- cos(ant2$angle)
u3 <- cos(ant3$angle)

v1 <- sin(ant1$angle)
v2 <- sin(ant2$angle)
v3 <- sin(ant3$angle)

plot1 <- ggplot(data.frame(x = x1, y = y1, u = u1, v = v1), aes(x = x1, y = y1, xend = x1 + u1, yend = y1 + v1)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "blue") +
  xlim(min(x1) - 0.5, max(x1) + 0.5) +
  ylim(min(y1) - 0.5, max(y1) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot : Indoor Colony Ant 11", x = "X", y = "Y")

plot2 <- ggplot(data.frame(x = x2, y = y2, u = u2, v = v2), aes(x = x2, y = y2, xend = x2 + u2, yend = y2 + v2)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "red") +
  xlim(min(x2) - 0.5, max(x2) + 0.5) +
  ylim(min(y2) - 0.5, max(y2) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot: Indoor Colony Ant 12", x = "X", y = "Y")

plot3 <- ggplot(data.frame(x = x3, y = y3, u = u3, v = v3), aes(x = x3, y = y3, xend = x3 + u3, yend = y3 + v3)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "green") +
  xlim(min(x3) - 0.5, max(x3) + 0.5) +
  ylim(min(y3) - 0.5, max(y3) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot: Indoor Colony Ant 13", x = "X", y = "Y")


g1 <- ggplotGrob(plot1)
g2 <- ggplotGrob(plot2)
g3 <- ggplotGrob(plot3)

combined <- grid.arrange(g1,g2,g3)

grid.newpage()
grid.draw(combined)

data <- read.table("ants/OutdoorDataset/Seq0006Object21Image64/gt/gt.txt", sep=",")
print(data)

ant4 <- ant_data(1)
ant5 <- ant_data(2)
ant6 <- ant_data(3)

x1 <- ant1$x[1:length(ant4$angle)]
x2 <- ant2$x[1:length(ant5$angle)]
x3 <- ant3$x[1:length(ant6$angle)]

y1 <- ant1$y[1:length(ant4$angle)]
y2 <- ant2$y[1:length(ant5$angle)]
y3 <- ant3$y[1:length(ant6$angle)]

u1 <- cos(ant4$angle)
u2 <- cos(ant5$angle)
u3 <- cos(ant6$angle)

v1 <- sin(ant4$angle)
v2 <- sin(ant5$angle)
v3 <- sin(ant6$angle)


plot4 <- ggplot(data.frame(x = x1, y = y1, u = u1, v = v1), aes(x = x1, y = y1, xend = x1 + u1, yend = y1 + v1)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "blue") +
  xlim(min(x1) - 0.5, max(x1) + 0.5) +
  ylim(min(y1) - 0.5, max(y1) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot : Outdoor Colony Ant 1", x = "X", y = "Y")

plot5 <- ggplot(data.frame(x = x2, y = y2, u = u2, v = v2), aes(x = x2, y = y2, xend = x2 + u2, yend = y2 + v2)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "red") +
  xlim(min(x2) - 0.5, max(x2) + 0.5) +
  ylim(min(y2) - 0.5, max(y2) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot: Outdoor Colony Ant 2", x = "X", y = "Y")

plot6 <- ggplot(data.frame(x = x3, y = y3, u = u3, v = v3), aes(x = x3, y = y3, xend = x3 + u3, yend = y3 + v3)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "green") +
  xlim(min(x3) - 0.5, max(x3) + 0.5) +
  ylim(min(y3) - 0.5, max(y3) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot: Outdoor Colony Ant 3", x = "X", y = "Y")


g4 <- ggplotGrob(plot4)
g5 <- ggplotGrob(plot5)
g6 <- ggplotGrob(plot6)

combined <- grid.arrange(g4,g5,g6)

grid.newpage()
grid.draw(combined)
