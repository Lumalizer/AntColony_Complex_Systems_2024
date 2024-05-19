library(ggplot2)
library(dplyr)
library(fields)
library(gridExtra)
library(grid)
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
    angle = c()
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
          print(i)
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
  }
  
  return(ant_list)
}

a1 <- ant_data(11)
a2 <- ant_data(12)
a3 <- ant_data(13)

x1 <- a1$x[1:350]
x2 <- a2$x[1:350]
x3 <- a3$x[1:350]

y1 <- a1$y[1:350]
y2 <- a2$y[1:350]
y3 <- a3$y[1:350]

u1 <- cos(a1$angle)
u2 <- cos(a2$angle)
u3 <- cos(a3$angle)

v1 <- sin(a1$angle)
v2 <- sin(a2$angle)
v3 <- sin(a3$angle)

plot1 <- ggplot(data.frame(x = x1, y = y1, u = u1, v = v1), aes(x = x1, y = y1, xend = x1 + u1, yend = y1 + v1)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "blue") +
  xlim(min(x1) - 0.5, max(x1) + 0.5) +
  ylim(min(y1) - 0.5, max(y1) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot", x = "X", y = "Y")

plot2 <- ggplot(data.frame(x = x2, y = y2, u = u2, v = v2), aes(x = x2, y = y2, xend = x2 + u2, yend = y2 + v2)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "red") +
  xlim(min(x2) - 0.5, max(x2) + 0.5) +
  ylim(min(y2) - 0.5, max(y2) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot", x = "X", y = "Y")

plot3 <- ggplot(data.frame(x = x3, y = y3, u = u3, v = v3), aes(x = x3, y = y3, xend = x3 + u3, yend = y3 + v3)) +
  geom_segment(arrow = arrow(length = unit(0.2, "inches")), color = "green") +
  xlim(min(x3) - 0.5, max(x3) + 0.5) +
  ylim(min(y3) - 0.5, max(y3) + 0.5) +
  theme_minimal() +
  labs(title = "Vector Field Plot", x = "X", y = "Y")


g1 <- ggplotGrob(plot1)
g2 <- ggplotGrob(plot2)
g3 <- ggplotGrob(plot3)

combined <- grid.arrange(g1,g2,g3)

grid.newpage()
grid.draw(combined)
