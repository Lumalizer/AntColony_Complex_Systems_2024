library(ggplot2)
library(dplyr)
library(rEDM)

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
  
  df <- df[-c(351), ]
  return(df)
}

a1 <- ant_data(11)

plot1 <- ggplot(data<-a1, aes(x=frame_no, y=angle)) + geom_line(aes(frame_no, angle),color="black") + geom_line(aes(frame_no,speed), color="blue")
plot1

plot2 <- ggplot(data<- a1, aes(x=angle, y= speed)) + geom_point() +ggtitle("ML and AP Scatterplot")
plot2

a1$angle <- as.numeric(scale(a1$angle))
a1$speed <- as.numeric(scale(a1$speed))

plot3 <- ggplot(data=a1, aes(x=frame_no, y = angle)) + geom_line(aes(frame_no,angle), color="black") + geom_line(aes(frame_no,speed), color="blue")
+ xlab("Frames") + ylab("Position") + ggtitle("Movement Time Series")
plot3

lib_point <- c(1,floor(max(length(a1$speed))/2))
pred_point <- c(floor(max(length(a1$speed))/2)+1,max(length(a1$speed)))

# Check for the embedding dimensions
rho_emd_ML <- EmbedDimension(dataFrame = a1, lib = lib_point, pred = pred_point, columns='speed', target ='speed')


simplex <- Simplex(dataFrame = a1, lib = lib_point, pred = pred_point, E=4, columns = 'angle', target = 'angle')

plot(simplex$Observations, type='l', xlab="Time", ylab="Value", main="Angle Simplex Projection")
lines(simplex$Predictions, type='l', col="blue")

ComputeError(simplex$Observations, simplex$Predictions)

rho_theta_ML<- PredictNonlinear(dataFrame = a1, lib = lib_point, pred = pred_point, E=, columns='speed', target ='speed')
#Theta - 3
